#' Extract figure and table paths from TeX files
#'
#' `getFigurePathsFromTex()` reads `\includegraphics{}` entries from a TeX file.
#' `getTablePathsFromTex()` reads `\input{}` entries and adds a `.tex`
#' extension when the TeX input omits it. Commented-out entries are ignored.
#'
#' Both functions follow nested `\input{}`, `\include{}`, and import-package
#' commands, with protection against circular includes. Standard includes resolve
#' from the report directory first, then the including file's directory. Imported
#' files and their assets resolve from the imported directory. Commands can span
#' lines; starred graphics commands and balanced arguments are supported.
#'
#' Definitions made with `\newcommand`, `\renewcommand`, `\providecommand`,
#' and `\def` (including simple arguments) are expanded in paths, in source order.
#' Undefined or recursive path macros warn and skip that reference. Use `.macros`
#' when definitions live outside the file tree being parsed. Inactive `\iffalse`,
#' `\iftrue`, `\ifdefined`, and `\newif` branches, `\includeonly` exclusions,
#' and verbatim examples are ignored.
#'
#' Figures honor `\graphicspath` and `\DeclareGraphicsExtensions`. Extensionless
#' images are matched to existing files using PDF graphics extension order by
#' default (PDF, PNG, JPEG, then other supported suffixes). If no matching image
#' exists, the extensionless path is returned with a warning.
#'
#' For tables, files in `.section_dirs` are wrappers to traverse. Other existing
#' files with report structure, definitions, or nested includes are also traversed.
#' Inputs inside table environments or directories named `table` or `tables`,
#' and leaf files containing tabular material, are treated as table assets.
#' Explicit `.section_dirs` takes precedence when the layout is ambiguous.
#' Missing section files are skipped; table assets need not exist to be listed.
#' This is a static reader, not a full TeX interpreter: arbitrary executable
#' macros, package-specific logic, and TeX grouping semantics are not evaluated.
#' Unsupported conditionals warn and skip their branches; simple `\ifthenelse`
#' equality and boolean tests are supported.
#'
#' @param .file TeX file to parse. Use the main report file to collect assets
#'   from all included sections, including methods and appendices.
#' @param .report_dir Directory that relative TeX paths should be resolved
#'   from. Defaults to the parent of a `sections` directory when `.file` is in
#'   one, otherwise `dirname(.file)`. Set explicitly when starting in another
#'   section directory.
#' @param .section_dirs Directories containing section wrappers, relative to
#'   `.report_dir` or absolute. Defaults to `"sections"`. Add custom directories
#'   to distinguish empty or not-yet-created sections from table assets.
#' @param .macros Named character vector of initial path macro values, without
#'   backslashes in the names, e.g. `c(runNumber = "106")`. Definitions in the
#'   parsed files can replace these values.
#'
#' @return Character vector of unique normalized paths, relative to the most
#'   specific containing directory among the working directory and `here::here()`,
#'   or absolute when neither contains the path.
#'
#' @examples
#' \dontrun{
#'   getFigurePathsFromTex(here::here("deliv/report/sections/figures.tex"))
#'   getTablePathsFromTex(here::here("deliv/report/sections/tables.tex"))
#' }
#' @export
getFigurePathsFromTex <- function(.file, .report_dir = NULL,
                                  .section_dirs = "sections", .macros = character()) {
  if (is.null(.report_dir)) {
    .report_dir <- tex_report_dir(.file)
  }
  tex_extract_paths(.file, "figure", .report_dir, .section_dirs, .macros)
}

#' @rdname getFigurePathsFromTex
#' @export
getTablePathsFromTex <- function(.file, .report_dir = NULL,
                                  .section_dirs = "sections", .macros = character()) {
  if (is.null(.report_dir)) {
    .report_dir <- tex_report_dir(.file)
  }
  tex_extract_paths(.file, "table", .report_dir, .section_dirs, .macros)
}

tex_report_dir <- function(.file) {
  d <- dirname(normalizePath(.file, winslash = "/", mustWork = FALSE))
  if (identical(basename(d), "sections")) dirname(d) else d
}

# A small static TeX reader: balanced arguments and source-order processing,
# without executing TeX or any commands embedded in the document.
tex_read_text <- function(file) {
  text <- paste(readLines(file, warn = FALSE), collapse = "\n")
  # Match comments and literal examples together, so a commented-out verbatim
  # delimiter cannot hide live text and a percent inside verbatim stays literal.
  pattern <- paste0(
    "(?s)<!--.*?-->|",
    "\\\\begin\\{(verbatim\\*?|Verbatim|lstlisting|minted|comment)\\}.*?\\\\end\\{\\1\\}|",
    "\\\\verb\\*?([^[:alnum:][:space:]]).*?\\2|",
    "(?m)(?<!\\\\)((?:\\\\\\\\)*)%[^\n]*(?:\n|$)"
  )
  stringr::str_replace_all(text, pattern, function(matches) {
    vapply(matches, function(match) {
      if (startsWith(match, "<!--") || startsWith(match, "\\begin") || startsWith(match, "\\verb")) {
        ""
      } else {
        # Preserve paired backslashes before a comment (e.g. a table row end).
        sub("%[\\s\\S]*$", "", match, perl = TRUE)
      }
    }, character(1), USE.NAMES = FALSE)
  })
}

tex_reader <- function(text) {
  r <- new.env(parent = emptyenv())
  r$text <- text
  r$pos <- 1L
  r$size <- nchar(text)
  r
}

tex_space <- function(r) {
  while (r$pos <= r$size && grepl("\\s", substr(r$text, r$pos, r$pos))) {
    r$pos <- r$pos + 1L
  }
}

tex_group <- function(r, open = "{", close = "}") {
  tex_space(r)
  if (substr(r$text, r$pos, r$pos) != open) return(NULL)
  r$pos <- r$pos + 1L
  start <- r$pos
  depth <- 1L
  while (r$pos <= r$size) {
    ch <- substr(r$text, r$pos, r$pos)
    if (ch == "\\") {
      r$pos <- r$pos + 2L
      next
    }
    if (ch == open) depth <- depth + 1L
    if (ch == close) depth <- depth - 1L
    if (depth == 0L) {
      value <- substr(r$text, start, r$pos - 1L)
      r$pos <- r$pos + 1L
      return(value)
    }
    r$pos <- r$pos + 1L
  }
  warning("Unclosed TeX argument; remaining text was skipped.", call. = FALSE)
  NULL
}

tex_command <- function(r) {
  if (substr(r$text, r$pos, r$pos) != "\\") return(NULL)
  rest <- substring(r$text, r$pos + 1L)
  name <- regmatches(rest, regexpr("^[A-Za-z@]+|^.", rest))
  if (!length(name)) return(NULL)
  r$pos <- r$pos + nchar(name) + 1L
  if (substr(r$text, r$pos, r$pos) == "*") r$pos <- r$pos + 1L
  name
}

tex_absolute <- function(path) grepl("^(/|[A-Za-z]:[/\\\\])", path)

tex_full_path <- function(path, root) {
  if (!tex_absolute(path)) path <- file.path(root, path)
  normalizePath(as.character(fs::path_norm(path)), winslash = "/", mustWork = FALSE)
}

tex_expand <- function(text, state, depth = 0L) {
  if (is.null(text)) return("")
  if (depth > 30L) {
    warning("Recursive TeX macro in a path; reference was skipped.", call. = FALSE)
    return(NA_character_)
  }
  r <- tex_reader(text)
  out <- character()
  while (r$pos <= r$size) {
    ch <- substr(text, r$pos, r$pos)
    if (ch != "\\") {
      out <- c(out, ch)
      r$pos <- r$pos + 1L
      next
    }
    name <- tex_command(r)
    if (name %in% c("%", "_", "#", "&", "{", "}")) {
      out <- c(out, name)
    } else if (exists(name, state$macros, inherits = FALSE)) {
      macro <- get(name, state$macros, inherits = FALSE)
      args <- character()
      if (!is.null(macro$default)) {
        arg <- tex_group(r, "[", "]")
        args <- c(args, if (is.null(arg)) macro$default else arg)
      }
      while (length(args) < macro$n) {
        arg <- tex_group(r)
        if (is.null(arg)) {
          warning(paste0("Missing argument for TeX macro \\", name,
            "; reference was skipped."), call. = FALSE)
          return(NA_character_)
        }
        args <- c(args, arg)
      }
      body <- macro$body
      for (i in rev(seq_along(args))) body <- gsub(paste0("#", i), args[[i]], body, fixed = TRUE)
      expanded <- tex_expand(body, state, depth + 1L)
      if (is.na(expanded)) return(NA_character_)
      out <- c(out, expanded)
      # TeX ignores whitespace following a control word.
      if (macro$n == 0L) tex_space(r)
    } else {
      warning(paste0("Unresolved TeX macro \\", name,
        " in a path; reference was skipped. Supply .macros or start from the main report."),
        call. = FALSE)
      return(NA_character_)
    }
  }
  paste(out, collapse = "")
}

tex_path_arg <- function(text, state) {
  value <- tex_expand(text, state)
  if (is.na(value)) return(value)
  value <- trimws(value)
  value <- sub("^['\"](.*)['\"]$", "\\1", value)
  # Braces used to delimit expanded macros are not part of the filename.
  gsub("[{}]", "", value)
}

tex_definition <- function(r, command, state) {
  tex_space(r)
  name <- tex_group(r)
  if (is.null(name)) name <- paste0("\\", tex_command(r))
  name <- sub("^\\\\", "", name)
  n <- 0L
  default <- NULL
  if (command %in% c("def", "gdef", "edef", "xdef")) {
    start <- r$pos
    while (r$pos <= r$size && substr(r$text, r$pos, r$pos) != "{") r$pos <- r$pos + 1L
    params <- substr(r$text, start, r$pos - 1L)
    nums <- regmatches(params, gregexpr("#[1-9]", params))[[1]]
    n <- length(nums)
  } else {
    count <- tex_group(r, "[", "]")
    if (!is.null(count)) n <- suppressWarnings(as.integer(count))
    default <- tex_group(r, "[", "]")
  }
  body <- tex_group(r)
  if (!is.null(body) && !is.na(n) && nzchar(name)) {
    if (command %in% c("edef", "xdef")) body <- tex_expand(body, state)
    if (!is.na(body) && !(command == "providecommand" && exists(name, state$macros, inherits = FALSE))) {
      assign(name, list(body = body, n = n, default = default), state$macros)
    }
  }
}

tex_resolve_input <- function(ref, file, root) {
  if (!grepl("\\.[[:alnum:]]+$", ref)) ref <- paste0(ref, ".tex")
  candidates <- unique(c(tex_full_path(ref, root), tex_full_path(ref, dirname(file))))
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) existing[[1]] else candidates[[1]]
}

tex_is_section <- function(file, state, in_table) {
  if (any(startsWith(file, paste0(state$section_dirs, "/")))) return(TRUE)
  if (in_table || grepl("/(table|tables)/", file)) return(FALSE)
  if (!file.exists(file)) return(FALSE)
  text <- tex_read_text(file)
  if (grepl("\\\\(section|subsection|chapter)\\b|\\\\begin\\s*\\{(table\\*?|figure\\*?)\\}",
    text, perl = TRUE)) return(TRUE)
  # A leaf tabular is an asset. A file containing report structure or further
  # includes is a wrapper, even when it lives outside the usual sections folder.
  if (grepl("\\\\begin\\s*\\{(tabular\\*?|tabularx|longtable|longtblr|tblr)\\}", text)) return(FALSE)
  !nzchar(trimws(text)) || grepl(
    "\\\\(input|include|includegraphics|import|subimport|section|subsection|chapter|newcommand|renewcommand|providecommand|def|gdef|edef|xdef|newif|graphicspath|includeonly|DeclareGraphicsExtensions)\\b|\\\\begin\\s*\\{(table\\*?|figure\\*?)\\}",
    text, perl = TRUE)
}

tex_figure_path <- function(ref, root, state) {
  roots <- unique(c(root, vapply(state$graphics, tex_full_path, character(1), root = root)))
  candidates <- unique(vapply(roots, function(base) tex_full_path(ref, base), character(1)))
  if (!grepl("\\.[[:alnum:]]+$", ref)) {
    candidates <- unlist(lapply(state$extensions, function(ext) paste0(candidates, ext)), use.names = FALSE)
  }
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) return(existing[[1]])
  if (!grepl("\\.[[:alnum:]]+$", ref)) {
    warning(paste0("Cannot determine image extension for '", ref,
      "': no matching file exists; returning the extensionless path."), call. = FALSE)
  }
  tex_full_path(ref, root)
}

tex_walk <- function(file, type, root, state, stack = character(), in_table = FALSE) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  if (file %in% stack) return(character())
  stack <- c(stack, file)
  r <- tex_reader(tex_read_text(file))
  paths <- character()
  conditions <- logical()
  table_depth <- as.integer(in_table)
  while (r$pos <= r$size) {
    if (substr(r$text, r$pos, r$pos) != "\\") {
      r$pos <- r$pos + 1L
      next
    }
    command <- tex_command(r)
    if (command == "ifdefined") {
      tex_space(r)
      name <- tex_command(r)
      conditions <- c(conditions, !is.null(name) && exists(name, state$macros, inherits = FALSE))
      next
    }
    if (command %in% c("iftrue", "iffalse") || exists(command, state$flags, inherits = FALSE)) {
      value <- if (command == "iftrue") TRUE else if (command == "iffalse") FALSE else get(command, state$flags)
      conditions <- c(conditions, value)
      next
    }
    if (command == "ifthenelse") {
      predicate <- tex_group(r)
      yes <- tex_group(r)
      no <- tex_group(r)
      if (length(conditions) && !isTRUE(all(conditions))) next
      probe <- tex_reader(if (is.null(predicate)) "" else predicate)
      tex_space(probe)
      test <- tex_command(probe)
      value <- NA
      if (identical(test, "equal")) {
        left <- tex_expand(tex_group(probe), state)
        right <- tex_expand(tex_group(probe), state)
        if (!is.na(left) && !is.na(right)) value <- identical(left, right)
      } else if (identical(test, "boolean")) {
        flag <- paste0("if", tex_group(probe))
        if (exists(flag, state$flags, inherits = FALSE)) value <- get(flag, state$flags)
      }
      tex_space(probe)
      if (probe$pos <= probe$size) value <- NA
      if (is.na(value)) {
        warning("Unsupported TeX ifthenelse test; both branches were skipped.", call. = FALSE)
      } else {
        branch <- if (value) yes else no
        r$text <- paste0(branch, substring(r$text, r$pos))
        r$pos <- 1L
        r$size <- nchar(r$text)
      }
      next
    }
    if (startsWith(command, "if")) {
      if (!length(conditions) || isTRUE(all(conditions))) {
        warning(paste0("Unsupported TeX conditional \\", command,
          "; its branches were skipped."), call. = FALSE)
      }
      conditions <- c(conditions, NA)
      next
    }
    if (command == "else" && length(conditions)) {
      conditions[length(conditions)] <- !conditions[length(conditions)]
      next
    }
    if (command == "fi" && length(conditions)) {
      conditions <- utils::head(conditions, -1L)
      next
    }
    if (length(conditions) && !isTRUE(all(conditions))) next
    if (command == "newif") {
      tex_space(r)
      flag <- tex_command(r)
      if (!is.null(flag)) assign(flag, FALSE, state$flags)
      next
    }
    if (grepl("(true|false)$", command)) {
      flag <- paste0("if", sub("(true|false)$", "", command))
      if (exists(flag, state$flags, inherits = FALSE)) {
        assign(flag, endsWith(command, "true"), state$flags)
        next
      }
    }
    if (command %in% c("newcommand", "renewcommand", "providecommand", "def", "gdef", "edef", "xdef")) {
      tex_definition(r, command, state)
      next
    }
    if (command %in% c("begin", "end")) {
      env <- tex_group(r)
      if (!is.null(env) && env %in% c("table", "table*", "longtable")) {
        table_depth <- max(0L, table_depth + if (command == "begin") 1L else -1L)
      }
      next
    }
    if (command == "graphicspath") {
      arg <- tex_group(r)
      if (!is.null(arg)) {
        arg <- tex_expand(arg, state)
        if (is.na(arg)) next
        groups <- tex_reader(arg)
        state$graphics <- character()
        repeat {
          path <- tex_group(groups)
          if (is.null(path)) break
          path <- tex_path_arg(path, state)
          if (!is.na(path)) state$graphics <- c(state$graphics, path)
        }
      }
      next
    }
    if (command %in% c("includeonly", "DeclareGraphicsExtensions")) {
      value <- tex_path_arg(tex_group(r), state)
      if (!is.na(value)) {
        values <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
        if (command == "includeonly") state$includeonly <- sub("\\.tex$", "", values) else state$extensions <- values
      }
      next
    }
    if (command == "includegraphics") {
      tex_group(r, "[", "]")
      tex_group(r, "[", "]")
      arg <- tex_group(r)
      if (type != "figure") next
      ref <- tex_path_arg(arg, state)
      if (type == "figure" && !is.na(ref) && nzchar(ref)) paths <- c(paths, tex_figure_path(ref, root, state))
      next
    }
    if (!command %in% c("input", "include", "import", "subimport", "inputfrom", "subinputfrom", "includefrom", "subincludefrom")) next
    import <- !command %in% c("input", "include")
    child_root <- root
    if (import) {
      directory <- tex_path_arg(tex_group(r), state)
      if (is.na(directory)) {
        tex_group(r)
        next
      }
      child_root <- tex_full_path(directory,
        if (startsWith(command, "sub")) root else state$report_dir)
    }
    arg <- tex_group(r)
    if (is.null(arg) && command == "input") {
      tex_space(r)
      rest <- substring(r$text, r$pos)
      arg <- regmatches(rest, regexpr("^[^[:space:]{}]+", rest))
      r$pos <- r$pos + nchar(arg)
    }
    ref <- tex_path_arg(arg, state)
    if (is.na(ref) || !nzchar(ref)) next
    is_include <- command %in% c("include", "includefrom", "subincludefrom")
    if (is_include && !is.null(state$includeonly) && !sub("\\.tex$", "", ref) %in% state$includeonly) next
    child <- tex_resolve_input(ref, file, child_root)
    section <- is_include || tex_is_section(child, state, table_depth > 0L)
    if (type == "table" && !section) {
      paths <- c(paths, child)
    } else if (file.exists(child)) {
      paths <- c(paths, tex_walk(child, type, child_root, state, stack, table_depth > 0L))
    }
  }
  paths
}

tex_extract_paths <- function(.file, type, report_dir, section_dirs = "sections", macros = character()) {
  report_dir <- normalizePath(report_dir, winslash = "/", mustWork = TRUE)
  state <- new.env(parent = emptyenv())
  state$macros <- new.env(parent = emptyenv())
  state$flags <- new.env(parent = emptyenv())
  state$report_dir <- report_dir
  if (!is.character(macros) || anyNA(macros) ||
      (length(macros) && (is.null(names(macros)) || anyNA(names(macros)) || any(!nzchar(names(macros)))))) {
    stop(".macros must be a named character vector.", call. = FALSE)
  }
  for (name in names(macros)) assign(name, list(body = macros[[name]], n = 0L, default = NULL), state$macros)
  if (!is.character(section_dirs) || anyNA(section_dirs)) {
    stop(".section_dirs must be a character vector without missing values.", call. = FALSE)
  }
  state$section_dirs <- vapply(section_dirs, tex_full_path, character(1), root = report_dir)
  state$graphics <- character()
  state$extensions <- c(".pdf", ".png", ".jpg", ".mps", ".jpeg", ".jbig2", ".jb2", ".PDF", ".PNG", ".JPG", ".JPEG", ".JBIG2", ".JB2", ".eps")
  state$includeonly <- NULL
  paths <- tex_walk(.file, type, report_dir, state)
  unique(tex_normalize_paths(paths, type, report_dir))
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
