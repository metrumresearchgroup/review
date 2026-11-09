# Compile a complete report in a temporary output directory. TeX, rather than
# a second implementation of its language, decides which branches/files are used.
tex_compile_paths <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  root <- dirname(file)
  header <- readLines(file, warn = FALSE)
  directive <- grep("^%\\s*!\\s*TeX\\s+program\\s*=", header,
    value = TRUE, ignore.case = TRUE, perl = TRUE)
  engines <- c("pdflatex", "lualatex", "xelatex")
  if (length(directive)) {
    engine <- tolower(trimws(sub("^[^=]*=", "", directive[[1]])))
    if (!engine %in% c(engines, "latex")) {
      stop("Unsupported TeX program directive: ", engine,
        ". Use pdflatex, lualatex, xelatex, or latex.", call. = FALSE)
    }
    engines <- engine
  }
  engines <- engines[nzchar(Sys.which(engines))]
  if (!length(engines)) {
    stop("No LaTeX engine found. Install LaTeX to compile the report.", call. = FALSE)
  }
  output <- tempfile("review-tex-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  output <- normalizePath(output, winslash = "/")
  # \include writes per-section auxiliary files. Mirror directory names only;
  # the report and its assets are still read from their original locations.
  dirs <- list.dirs(root, full.names = FALSE, recursive = TRUE)
  dirs <- dirs[nzchar(dirs) & !grepl("(^|/)\\.", dirs)]
  for (dir in dirs) dir.create(file.path(output, dir), recursive = TRUE, showWarnings = FALSE)
  job <- tools::file_path_sans_ext(basename(file))
  driver <- file.path(output, "review-driver.tex")
  hooks <- system.file("tex", "review-assets.tex", package = "review")
  if (!nzchar(hooks)) stop("Missing review LaTeX instrumentation.", call. = FALSE)
  # detokenize protects ordinary filenames; special characters are generated
  # separately so percent signs cannot become comments in the driver itself.
  parts <- strsplit(file, "", fixed = TRUE)[[1]]
  encoded <- vapply(parts, function(ch) {
    if (ch %in% c("%", "{", "}", "#")) paste0("\\string\\", ch) else paste0("\\detokenize{", ch, "}")
  }, character(1))
  writeLines(c(readLines(hooks, warn = FALSE),
    "\\begingroup\\escapechar=-1",
    paste0("\\xdef\\reviewEntry{", paste(encoded, collapse = ""), "}"),
    "\\endgroup", "\\expandafter\\input\\expandafter{\\reviewEntry}"), driver)
  failures <- character()
  for (engine in engines) {
    # Start each engine with clean auxiliary state from the previous attempt.
    generated <- list.files(output, recursive = TRUE, full.names = TRUE)
    unlink(setdiff(generated, driver))
    previous <- NULL
    for (pass in seq_len(4L)) {
      result <- tryCatch(processx::run(Sys.which(engine), c(
        "-no-shell-escape", "-halt-on-error", "-interaction=nonstopmode",
        "-recorder", paste0("-output-directory=", output),
        paste0("-jobname=", job), driver
      ), wd = root, timeout = 120, error_on_status = FALSE), error = identity)
      if (inherits(result, "error")) {
        failures <- c(failures, paste0(engine, ": ", conditionMessage(result)))
        break
      }
      if (result$status != 0L) {
        lines <- strsplit(result$stdout, "\n", fixed = TRUE)[[1]]
        errors <- grep("^!", lines)
        detail <- if (length(errors)) {
          paste(lines[seq.int(errors[[1]], min(length(lines), errors[[1]] + 3L))], collapse = "\n")
        } else paste(utils::tail(lines, 6L), collapse = "\n")
        failures <- c(failures, paste0(engine, ":\n", detail))
        break
      }
      auxiliary <- list.files(output, pattern = "\\.(aux|toc|lof|lot|out)$",
        full.names = TRUE, recursive = TRUE)
      current <- tools::md5sum(auxiliary)
      if (identical(current, previous)) {
        return(tex_compiled_assets(
          file.path(output, paste0(job, ".review-assets")),
          file.path(output, paste0(job, ".fls")), root, file, output))
      }
      previous <- current
      if (pass == 4L) {
        failures <- c(failures, paste0(engine, ": auxiliary files did not stabilize after four passes."))
      }
    }
  }
  stop("Could not compile the report; no partial asset list was returned.\n",
    paste(failures, collapse = "\n"),
    "\nFix the report build and try again.",
    call. = FALSE)
}

tex_compiled_assets <- function(trace_file, recorder_file, root, main, output) {
  if (!file.exists(trace_file) || !file.exists(recorder_file)) {
    stop("LaTeX did not produce an asset trace and file recorder.", call. = FALSE)
  }
  trace <- readLines(trace_file, warn = FALSE)
  if (!"Z|" %in% trace) stop("LaTeX asset trace is incomplete.", call. = FALSE)
  recorded <- readLines(recorder_file, warn = FALSE)
  recorded <- unique(sub("^INPUT ", "", grep("^INPUT ", recorded, value = TRUE)))
  recorded <- unique(vapply(recorded, tex_full_path, character(1), root = root))
  resolve <- function(path) {
    path <- sub('^"(.*)"$', "\\1", trimws(path))
    direct <- tex_full_path(path, root)
    if (direct %in% recorded) return(direct)
    candidates <- recorded[endsWith(recorded, paste0("/", path))]
    if (length(candidates) == 1L) return(candidates[[1]])
    if (file.exists(direct)) return(direct)
    stop("Cannot uniquely resolve recorded asset: ", path, call. = FALSE)
  }
  # Recorder entries alone are not evidence of figure use: IfFileExists probes
  # also open files. Only graphics events identify figure assets.
  figures <- character()
  tables <- character()
  stack <- character()
  table_depth <- 0L
  for (line in trace) {
    event <- substr(line, 1L, 1L)
    path <- substring(line, 3L)
    if (event == "B") {
      support_file <- grepl("\\.(sty|cls|clo|cfg|def|fd|ldf|aux|toc|out|lof|lot|bbl|bst)$",
        path, ignore.case = TRUE)
      candidate <- if (support_file) NA_character_ else resolve(path)
      nested_asset <- any(stack %in% tables)
      stack <- c(stack, candidate)
      if (table_depth > 0L && !is.na(candidate) && candidate != main && !nested_asset) {
        tables <- c(tables, candidate)
      }
    } else if (event == "E") {
      stack <- utils::head(stack, -1L)
    } else if (event == "G") {
      figures <- c(figures, resolve(path))
    } else if (event %in% c("A", "T")) {
      if (event == "A" && length(stack) && !any(stack %in% tables)) {
        candidate <- utils::tail(stack, 1L)
        if (!is.na(candidate) && candidate != main) tables <- c(tables, candidate)
      }
      table_depth <- table_depth + 1L
    } else if (event %in% c("a", "t")) {
      table_depth <- max(0L, table_depth - 1L)
    }
  }
  assets <- unique(c(figures, tables))
  if (any(startsWith(assets, paste0(output, "/")))) {
    stop("The report generates assets inside the temporary build directory; ",
      "build those assets in the project before extracting their paths.", call. = FALSE)
  }
  list(figures = unique(figures), tables = unique(tables))
}

tex_full_path <- function(path, root) {
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) path <- file.path(root, path)
  normalizePath(as.character(fs::path_norm(path)), winslash = "/", mustWork = FALSE)
}
