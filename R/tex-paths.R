#' Extract figure and table paths from TeX files
#'
#' `getFigurePathsFromTex()` reads `\includegraphics{}` entries from a TeX file.
#' `getTablePathsFromTex()` reads `\input{}` entries and adds a `.tex`
#' extension when the TeX input omits it. Commented-out entries are ignored.
#'
#' @param .file TeX file to parse.
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
  args[nzchar(args)]
}

tex_extract_paths <- function(.file, type, report_dir, recurse = TRUE) {
  lines <- tex_read_lines(.file)
  command <- if (identical(type, "figure")) "includegraphics" else "input"
  paths <- tex_command_args(lines, command)

  sub_paths <- if (identical(type, "figure") && isTRUE(recurse)) {
    tex_sub_paths(.file, type, report_dir, lines)
  } else {
    character()
  }

  unique(c(tex_normalize_paths(paths, type, report_dir), sub_paths))
}

# \input{} in a figures file points to a sub-section TeX file, not a figure.
# Recurse into any that resolve to an existing .tex file (one level only).
tex_sub_paths <- function(.file, type, report_dir, lines) {
  refs <- tex_command_args(lines, "input")
  if (!length(refs)) {
    return(character())
  }

  no_ext <- !grepl("\\.[[:alnum:]]+$", refs)
  refs[no_ext] <- paste0(refs[no_ext], ".tex")

  sub_files <- ifelse(
    grepl("^(/|[A-Za-z]:[/\\\\])", refs),
    refs,
    file.path(dirname(.file), refs)
  )
  sub_files <- sub_files[file.exists(sub_files)]

  purrr::map(
    sub_files,
    tex_extract_paths,
    type = type,
    report_dir = report_dir,
    recurse = FALSE
  ) %>%
    purrr::list_c()
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
