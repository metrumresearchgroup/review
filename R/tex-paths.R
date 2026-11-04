#' Extract figure and table paths from TeX files
#'
#' `getFigurePathsFromTex()` reads `\includegraphics{}` entries from a TeX file.
#' `getTablePathsFromTex()` reads `\input{}` entries and adds a `.tex`
#' extension when the TeX input omits it. Commented-out entries are ignored.
#'
#' Both functions follow nested `\input{}` and `\include{}` section files,
#' with protection against circular includes. Includes resolve from the report
#' directory first, then from the including file's directory as a fallback.
#' For tables, files under `.report_dir/sections/` are section wrappers to
#' traverse, not table assets; inputs outside that directory are returned as
#' table paths, whether or not those assets exist. Missing section files are
#' skipped. Figure extraction follows all existing included files.
#'
#' @param .file TeX file to parse. Use the main report file to collect assets
#'   from all included sections, including methods and appendices.
#' @param .report_dir Directory that relative TeX paths should be resolved
#'   from. Defaults to the parent of a `sections` directory when `.file` is in
#'   one, otherwise `dirname(.file)`.
#'
#' @return Character vector of normalized paths relative to the current working
#'   directory when possible, otherwise relative to `here::here()`.
#'
#' @examples
#' \dontrun{
#'   getFigurePathsFromTex(here::here("deliv/report/sections/figures.tex"))
#'   getTablePathsFromTex(here::here("deliv/report/sections/tables.tex"))
#' }
#' @export
getFigurePathsFromTex <- function(.file, .report_dir = NULL) {
  if (is.null(.report_dir)) {
    .report_dir <- tex_report_dir(.file)
  }
  tex_extract_paths(.file, "figure", .report_dir)
}

#' @rdname getFigurePathsFromTex
#' @export
getTablePathsFromTex <- function(.file, .report_dir = NULL) {
  if (is.null(.report_dir)) {
    .report_dir <- tex_report_dir(.file)
  }
  tex_extract_paths(.file, "table", .report_dir)
}

tex_report_dir <- function(.file) {
  d <- dirname(normalizePath(.file, winslash = "/", mustWork = FALSE))
  if (identical(basename(d), "sections")) dirname(d) else d
}

tex_read_lines <- function(.file) {
  lines <- readLines(.file, warn = FALSE)
  text <- paste(lines, collapse = "\n")
  text <- gsub("(?s)<!--.*?-->", "", text, perl = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  sub("(^|[^\\\\])%.*$", "\\1", lines, perl = TRUE)
}

tex_command_args <- function(lines, command) {
  pattern <- paste0("\\\\", command, "\\s*(?:\\[[^]]*\\]\\s*)?\\{([^{}]+)\\}")
  raw <- unlist(
    regmatches(lines, gregexpr(pattern, lines, perl = TRUE)),
    use.names = FALSE
  )
  if (!length(raw)) {
    return(character())
  }
  args <- stringr::str_trim(stringr::str_remove_all(
    sub(pattern, "\\1", raw, perl = TRUE),
    '^["\']|["\']$'
  ))
  
  # Remove LaTeX macro backslashes so path normalizers don't convert them to '/'
  args <- gsub("\\\\", "", args)
  
  args[nzchar(args)]
}

tex_extract_paths <- function(.file, type, report_dir, visited = character()) {
  file <- normalizePath(.file, winslash = "/", mustWork = TRUE)
  if (file %in% visited) {
    return(character())
  }
  visited <- c(visited, file)
  report_dir <- normalizePath(report_dir, winslash = "/", mustWork = FALSE)
  lines <- tex_read_lines(file)
  inputs <- tex_command_args(lines, "input")
  includes <- tex_command_args(lines, "include")
  refs <- c(inputs, includes)
  sub_files <- vapply(refs, tex_resolve_input, character(1),
    file = file, report_dir = report_dir, USE.NAMES = FALSE)

  if (identical(type, "figure")) {
    paths <- tex_command_args(lines, "includegraphics")
    traverse <- rep(TRUE, length(refs))
  } else {
    # Section files organize the report; inputs elsewhere are table assets.
    sections <- normalizePath(file.path(report_dir, "sections"),
      winslash = "/", mustWork = FALSE)
    traverse <- startsWith(sub_files, paste0(sections, "/"))
    paths <- inputs[!traverse[seq_along(inputs)]]
  }

  sub_paths <- lapply(sub_files[traverse & file.exists(sub_files)],
    tex_extract_paths, type = type, report_dir = report_dir, visited = visited)
  unique(c(tex_normalize_paths(paths, type, report_dir),
    unlist(sub_paths, use.names = FALSE)))
}

# TeX includes are normally relative to the report root. Keep sibling lookup
# as a fallback for section files that use bare names such as figures-pk.
tex_resolve_input <- function(ref, file, report_dir) {
  if (!grepl("\\.[[:alnum:]]+$", ref)) {
    ref <- paste0(ref, ".tex")
  }
  candidates <- if (grepl("^(/|[A-Za-z]:[/\\\\])", ref)) {
    ref
  } else {
    c(file.path(report_dir, ref), file.path(dirname(file), ref))
  }
  existing <- candidates[file.exists(candidates)]
  path <- if (length(existing)) existing[[1]] else candidates[[1]]
  normalizePath(as.character(fs::path_norm(path)),
    winslash = "/", mustWork = FALSE)
}

tex_normalize_paths <- function(paths, type, report_dir) {
  if (identical(type, "table")) {
    no_ext <- !grepl("\\.[[:alnum:]]+$", paths)
    paths[no_ext] <- paste0(paths[no_ext], ".tex")
  }

  if (!length(paths)) {
    return(character())
  }

  roots <- unique(normalizePath(
    c(getwd(), here::here()),
    winslash = "/",
    mustWork = FALSE
  ))

  purrr::map_chr(paths, function(p) {
    abs <- if (grepl("^(/|[A-Za-z]:[/\\\\])", p)) {
      p
    } else {
      file.path(report_dir, p)
    }
    abs <- normalizePath(
      as.character(fs::path_norm(abs)),
      winslash = "/",
      mustWork = FALSE
    )
    rel_roots <- roots[startsWith(abs, paste0(roots, "/")) | abs == roots]
    if (!length(rel_roots)) {
      return(abs)
    }
    root <- rel_roots[[which.max(nchar(rel_roots))]]
    stringr::str_remove(abs, paste0("^", stringr::str_escape(root), "/?"))
  })
}
