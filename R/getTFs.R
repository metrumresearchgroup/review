utils::globalVariables(c("source_graphic", "source_code", "path", "type"))

#' Extract file path of tables and figures
#' 
#' @description
#' Parses a PDF and extracts the file paths printed in lines beginning with
#' \code{Source graphic:} (figures) and \code{Source file:} (tables), along with
#' the most recently encountered \code{Source code:} path. Page numbers are
#' retained to help trace where each entry was found. The returned data frame
#' includes row-level checks for whether the artifact path exists, whether the
#' source code path exists, whether the artifact was found in the PDF, and
#' whether it was listed in supplied or auto-discovered TeX files.
#' 
#' @param .file File path of PDF containing tables and figures
#' @param .figures_tex Optional file path to a LaTeX file containing
#'   `\includegraphics{}` entries used to validate and correct extracted figure
#'   paths. The default, `"auto"`, looks for `sections/figures.tex` next to
#'   `.file`. TeX-derived paths are normalized relative to the current working
#'   directory when possible, otherwise relative to `here::here()`.
#'   Use `NULL` to skip figure TeX validation.
#' @param .tables_tex Optional file path to a LaTeX file containing `\input{}`
#'   entries used to validate and correct extracted table paths. The default,
#'   `"auto"`, looks for `sections/tables.tex` next to `.file`. TeX-derived
#'   paths are normalized relative to the current working directory when
#'   possible, otherwise relative to `here::here()`. Use `NULL` to skip table
#'   TeX validation.
#' 
#' @examples
#' \dontrun{
#'   getTFs(here::here("deliv/report/pk-report.pdf"))
#'   getTFs(
#'     here::here("deliv/report/report.pdf"),
#'     .figures_tex = here::here("deliv/report/sections/figures.tex"),
#'     .tables_tex = here::here("deliv/report/sections/tables.tex")
#'   )
#' }
#' @export
getTFs <- function(.file, .figures_tex = "auto", .tables_tex = "auto") {
  tex_paths <- report_tex_paths(.file, .figures_tex, .tables_tex)
  source_roots <- source_info_roots(.file)
  
  # Read PDF into lines
  pdf.text_orig <- pdftools::pdf_text(.file)
  
  # Normalize Unicode to fix ligatures and other compatibility characters
  pdf.text_orig <- stringi::stri_trans_nfkc(pdf.text_orig)
  
  # Split into list to preserve page structure temporarily
  pdf.list <- strsplit(pdf.text_orig, "\n")
  
  # Create a map: repeats the page number for every line on that page
  page_map <- rep(seq_along(pdf.list), lengths(pdf.list))
  
  # Now flatten into a single vector of lines
  pdf.text <- unlist(pdf.list)
  pdf.text <- trimws(pdf.text)
  
  # Identify lines
  fig_idx   <- grep("^Source graphic:", pdf.text)
  tab_idx   <- grep("^Source file:", pdf.text)
  code_idx  <- grep("^Source code:", pdf.text)
  
  # Extract text for each
  figs  <- pdf.text[fig_idx]
  tabs  <- pdf.text[tab_idx]
  codes <- pdf.text[code_idx]
  
  figs_w_idx <-
    dplyr::tibble(
      idx = fig_idx,
      source_graphic = figs,
      page = page_map[fig_idx],
      type = "figure"
    )

  tabs_w_idx <-
    dplyr::tibble(
      idx = tab_idx,
      source_graphic = tabs,
      page = page_map[tab_idx],
      type = "table"
    )

  code_w_idx <-
    dplyr::tibble(
      idx = code_idx,
      source_code = codes,
      page = page_map[code_idx],
      type = "code"
    )

  combine1 <-
    dplyr::bind_rows(figs_w_idx, tabs_w_idx, code_w_idx) %>%
    dplyr::arrange(idx) %>%
    dplyr::select(page, type, dplyr::everything())

  combine2 <-
    combine1 %>%
    tidyr::fill(source_code, .direction = "down") %>%
    dplyr::filter(!is.na(source_graphic))

  df <- combine2

  # Remove prefixes for cleaner output (optional)
  df$source_graphic <- sub("^Source graphic:\\s*", "", df$source_graphic)
  df$source_graphic <- sub("^Source file:\\s*", "", df$source_graphic)
  df$source_code   <- sub("^Source code:\\s*", "", df$source_code)
  df$source_code <- gsub("\u2212", "-", df$source_code)
  df$source_graphic      <- gsub("\u2212", "-", df$source_graphic)
  df$idx <- NULL
  
  image_df <- extract_pdf_image_tfs(
    .file,
    pages_with_text_paths = unique(page_map[c(fig_idx, tab_idx)]),
    source_root = source_roots,
    tex_paths = tex_paths
  )

  df <- dplyr::bind_rows(df, image_df) %>%
    dplyr::arrange(page)
  df <- validate_tf_paths_with_tex(df, tex_paths)
  df <- add_tf_diagnostics(df, .file, tex_paths, source_roots)
  tex_only_df <- build_tex_only_rows(df, tex_paths)

  in_tex <- !is.na(df$source_graphic_in_tex) & df$source_graphic_in_tex
  pdf_and_tex <- as.data.frame(df[in_tex, cols_pdf()])
  pdf_only    <- as.data.frame(df[!in_tex, cols_pdf()])
  tex_only    <- as.data.frame(tex_only_df[, cols_tex()])

  list(pdf_and_tex = pdf_and_tex, pdf_only = pdf_only, tex_only = tex_only)
}

cols_pdf <- function() {
  c("page", "type", "source_graphic", "source_code", "source_graphic_exists", "source_code_exists")
}

cols_tex <- function() {
  c("type", "source_graphic", "source_graphic_exists")
}

extract_pdf_image_tfs <- function(
  .file,
  pages_with_text_paths,
  source_root = NULL,
  tex_paths = empty_report_tex_paths()
) {
  # Only OCR pages that actually contain a raster image and were not already
  # captured from the PDF text. This avoids rendering and OCR'ing image-free
  # pages, which is the dominant cost.
  candidate_pages <- setdiff(pdf_image_pages(.file), pages_with_text_paths)

  if (!length(candidate_pages)) {
    return(empty_tf_tbl())
  }

  # Figure paths from the report TeX, used to confirm extracted graphic paths
  # against ground truth and stop OCR escalation once a page is fully resolved.
  tex_graphics <- tex_paths$path[tex_paths$type == "figure"]

  # Pre-scan source files once so workers don't each repeat the directory walk.
  roots <- normalize_source_roots(source_root)
  source_files <- if (length(roots)) source_files_under_roots(roots) else NULL

  # Leave one core free for the OS / interactive session.
  n_workers <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

  oopts <- options(parallelly.supportsMulticore.unstable = "quiet")
  on.exit(options(oopts), add = TRUE)
  oplan <- future::plan(future::multicore, workers = n_workers)
  on.exit(future::plan(oplan), add = TRUE)

  progressr::with_progress({
    p <- progressr::progressor(along = candidate_pages)

    rows <- future.apply::future_lapply(candidate_pages, function(page) {
      p(message = sprintf("page %d", page))

      img <- magick::image_read(
        pdftools::pdf_render_page(.file, page = page, dpi = 216)
      )
      source_info <- extract_source_info(
        img,
        crop_fraction = 0.45,
        line_ocr = TRUE,
        source_root = source_root,
        tex_graphics = tex_graphics,
        source_files = source_files
      )

      if (is.na(source_info$Source_Code) || is.na(source_info$Source_Graphic)) {
        return(NULL)
      }
      source_info$Source_Graphic <- correct_report_tex_path(
        source_info$Source_Graphic,
        "figure",
        tex_paths
      )

      dplyr::tibble(
        page = as.integer(page),
        type = "figure",
        source_graphic = source_info$Source_Graphic,
        source_code = source_info$Source_Code
      )
    }, future.seed = TRUE)
  })

  rows <- purrr::compact(rows)
  if (!length(rows)) {
    return(empty_tf_tbl())
  }

  dplyr::bind_rows(rows)
}

# Pages of a PDF that contain an embedded raster image, via the poppler
# `pdfimages` CLI. The `-list` output has two header lines followed by one row
# per image: `<page> <num> <type> ...`. We keep rows whose type column is
# `image` (ignoring `smask` soft-mask rows) and return their page numbers.
pdf_image_pages <- function(.file) {
  if (!nzchar(Sys.which("pdfimages"))) return(integer(0))
  listing <- processx::run("pdfimages", c("-list", .file))$stdout
  listing <- strsplit(listing, "\n")[[1]]
  listing <- listing[-(1:2)]
  image_rows <- grepl("^\\s*\\d+\\s+\\d+\\s+image\\b", listing)
  pages <- as.integer(sub("^\\s*(\\d+)\\s.*", "\\1", listing[image_rows]))
  sort(unique(pages[!is.na(pages)]))
}

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
getFigurePathsFromTex <- function(
  .file,
  .report_dir = NULL
) {
  if (is.null(.report_dir)) {
    .report_dir <- report_dir_from_tex(.file)
  }

  extract_report_tex_paths(.file, "figure", .report_dir)
}

#' @rdname getFigurePathsFromTex
#' @export
getTablePathsFromTex <- function(
  .file,
  .report_dir = NULL
) {
  if (is.null(.report_dir)) {
    .report_dir <- report_dir_from_tex(.file)
  }

  extract_report_tex_paths(.file, "table", .report_dir)
}

report_tex_paths <- function(.file, .figures_tex = "auto", .tables_tex = "auto") {
  report_dir <- normalizePath(dirname(.file), winslash = "/", mustWork = FALSE)

  dplyr::bind_rows(
    read_report_tex_paths(.figures_tex, "figure", report_dir, ".figures_tex"),
    read_report_tex_paths(.tables_tex, "table", report_dir, ".tables_tex")
  )
}

report_path_roots <- function() {
  roots <- c(getwd(), here::here())
  roots <- roots[!is.na(roots) & nzchar(roots)]
  unique(normalizePath(roots, winslash = "/", mustWork = FALSE))
}

report_relative_path <- function(path) {
  source_relative_path(path, report_path_roots())
}

read_report_tex_paths <- function(tex_file, type, report_dir, arg_name) {
  if (is.null(tex_file) || is.na(tex_file) || !nzchar(tex_file)) {
    return(empty_report_tex_paths())
  }

  tex_file <- resolve_report_tex_file(tex_file, type, report_dir, arg_name)
  if (is.null(tex_file)) {
    return(empty_report_tex_paths())
  }

  paths <- if (identical(type, "figure")) {
    getFigurePathsFromTex(
      tex_file,
      .report_dir = report_dir
    )
  } else {
    getTablePathsFromTex(
      tex_file,
      .report_dir = report_dir
    )
  }

  if (!length(paths)) {
    return(empty_report_tex_paths())
  }

  dplyr::tibble(
    type = type,
    path = unique(paths)
  )
}

resolve_report_tex_file <- function(tex_file, type, report_dir, arg_name) {
  file_name <- paste0(type, "s.tex")
  auto <- identical(tex_file, "auto")

  if (auto) {
    path <- normalizePath(
      file.path(report_dir, "sections", file_name),
      winslash = "/",
      mustWork = FALSE
    )
  } else if (source_path_is_absolute(tex_file)) {
    path <- normalizePath(tex_file, winslash = "/", mustWork = FALSE)
  } else {
    path <- normalizePath(file.path(report_dir, tex_file), winslash = "/", mustWork = FALSE)
  }

  if (file.exists(path)) {
    cli::cli_inform("Using TeX validation file for {type}s: {.path {report_relative_path(path)}}")
    return(path)
  }

  if (!auto && !source_path_is_absolute(tex_file) && file.exists(tex_file)) {
    path <- normalizePath(tex_file, winslash = "/", mustWork = FALSE)
    cli::cli_inform("Using TeX validation file for {type}s: {.path {report_relative_path(path)}}")
    return(path)
  }

  if (auto) {
    cli::cli_inform(
      "Optional TeX validation file was not found for {type}s: {.path {report_relative_path(path)}}. Provide `{arg_name}` to use a different file."
    )
  } else {
    cli::cli_inform("TeX validation file does not exist: {.path {report_relative_path(path)}}")
  }
  NULL
}

extract_report_tex_paths <- function(.file, type, .report_dir) {
  tex <- strip_latex_comments(readLines(.file, warn = FALSE))
  command <- if (identical(type, "figure")) "includegraphics" else "input"
  paths <- extract_latex_command_paths(tex, command)
  paths <- normalize_report_tex_paths(paths, type, .report_dir)

  unique(paths)
}

strip_latex_comments <- function(x) {
  sub("(^|[^\\\\])%.*$", "\\1", x, perl = TRUE)
}

extract_latex_command_paths <- function(x, command) {
  pattern <- paste0("\\\\", command, "\\s*(?:\\[[^]]*\\]\\s*)?\\{([^{}]+)\\}")
  matches <- gregexpr(pattern, x, perl = TRUE)
  values <- regmatches(x, matches)
  values <- unlist(values, use.names = FALSE)
  if (!length(values)) {
    return(character())
  }

  sub(pattern, "\\1", values, perl = TRUE)
}

normalize_report_tex_paths <- function(paths, type, report_dir) {
  paths <- trimws(paths)
  paths <- gsub('^["\']|["\']$', "", paths)
  paths <- paths[nzchar(paths)]

  if (identical(type, "table")) {
    no_extension <- !grepl("\\.[[:alnum:]]+$", paths)
    paths[no_extension] <- paste0(paths[no_extension], ".tex")
  }

  paths <- purrr::map_chr(paths, function(path) {
    absolute_path <- if (source_path_is_absolute(path)) {
      path
    } else {
      file.path(report_dir, path)
    }
    absolute_path <- as.character(fs::path_norm(absolute_path))
    report_relative_path(
      normalizePath(absolute_path, winslash = "/", mustWork = FALSE)
    )
  })

  unname(paths)
}

report_dir_from_tex <- function(.file) {
  tex_dir <- dirname(normalizePath(.file, winslash = "/", mustWork = FALSE))
  if (identical(basename(tex_dir), "sections")) {
    return(dirname(tex_dir))
  }

  tex_dir
}

validate_tf_paths_with_tex <- function(df, tex_paths) {
  if (!nrow(tex_paths) || !nrow(df)) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      source_graphic = purrr::map2_chr(
        source_graphic,
        type,
        correct_report_tex_path,
        tex_paths = tex_paths
      )
    )
}

add_tf_diagnostics <- function(df, .file, tex_paths, source_roots) {
  if (!nrow(df)) {
    df$page <- integer()
    df$type <- character()
    df$source_graphic <- character()
    df$source_code <- character()
    df$source_graphic_exists <- logical()
    df$source_graphic_in_tex <- logical()
    df$source_code_exists <- logical()
    return(df[, c(
      "page", "type", "source_graphic", "source_code",
      "source_graphic_exists", "source_graphic_in_tex", "source_code_exists"
    )])
  }

  df <- df %>%
    dplyr::mutate(
      source_graphic_exists = purrr::map_lgl(
        source_graphic,
        report_artifact_exists,
        project_root = report_path_roots()
      ),
      source_graphic_in_tex = purrr::map2_lgl(
        source_graphic,
        type,
        report_path_in_tex,
        tex_paths = tex_paths
      ),
      source_code_exists = purrr::map_lgl(
        source_code,
        source_code_exists_check,
        source_roots = source_roots
      )
    )
  df$source_graphic_in_tex[purrr::map_lgl(df$type, report_tex_type_missing, tex_paths = tex_paths)] <- NA

  df[, c(
    "page", "type", "source_graphic", "source_code",
    "source_graphic_exists", "source_graphic_in_tex", "source_code_exists"
  )]
}

build_tex_only_rows <- function(df, tex_paths) {
  if (!nrow(tex_paths)) {
    return(dplyr::tibble(type = character(), source_graphic = character(), source_graphic_exists = logical()))
  }

  existing_paths <- tf_base_path(df$source_graphic)
  tex_only <- tex_paths %>%
    dplyr::filter(!path %in% existing_paths)

  if (!nrow(tex_only)) {
    return(dplyr::tibble(type = character(), source_graphic = character(), source_graphic_exists = logical()))
  }

  tex_only %>%
    dplyr::transmute(
      type,
      source_graphic = path,
      source_graphic_exists = purrr::map_lgl(
        path,
        report_artifact_exists,
        project_root = report_path_roots()
      )
    )
}

report_artifact_exists <- function(path, project_root) {
  if (is.na(path) || !nzchar(path)) {
    return(FALSE)
  }

  base_path <- tf_base_path(path)
  if (source_path_is_absolute(base_path)) {
    return(file.exists(base_path))
  }

  project_root <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  any(file.exists(file.path(project_root, base_path)))
}

report_path_in_tex <- function(path, type, tex_paths) {
  candidates <- tex_paths$path[tex_paths$type == type]
  if (!length(candidates) || is.na(path)) {
    return(FALSE)
  }

  base_path <- tf_base_path(path)
  base_path %in% candidates
}

report_tex_type_missing <- function(type, tex_paths) {
  !any(tex_paths$type == type)
}

source_code_exists_check <- function(path, source_roots) {
  if (is.na(path) || !nzchar(path)) {
    return(FALSE)
  }

  resolution <- resolve_source_code_candidates(path, source_roots)
  !is.na(resolution$path[[1]])
}

correct_report_tex_path <- function(path, type, tex_paths) {
  candidates <- tex_paths$path[tex_paths$type == type]
  if (!length(candidates) || is.na(path)) {
    return(path)
  }

  suffix <- tf_page_suffix(path)
  base_path <- tf_base_path(path)

  page_num <- tf_page_number(suffix)
  if (!is.na(page_num) && page_num > 1) {
    base_stem <- sub("\\.[[:alnum:]]+$", "", base_path)
    expected_stem <- paste0(base_stem, "-page", page_num)
    expected_basename_stem <- basename(expected_stem)
    candidate_stems <- sub("\\.[[:alnum:]]+$", "", candidates)
    exact_paged <- candidates[
      candidate_stems == expected_stem |
        basename(candidate_stems) == expected_basename_stem
    ]
    if (length(exact_paged)) {
      return(exact_paged[[1]])
    }
  }

  match <- closest_path_match(base_path, candidates, same_ext = TRUE)
  if (is.na(match$path)) {
    match <- closest_path_match(base_path, candidates, same_ext = FALSE)
  }

  paste0(
    if (is.na(match$path)) base_path else match$path,
    ifelse(is.na(suffix), "", suffix)
  )
}

tf_base_path <- function(path) {
  stringr::str_remove(path, "(?i)\\s+page:\\s+\\d+$")
}

tf_page_suffix <- function(path) {
  stringr::str_extract(path, "(?i)\\s+page:\\s+\\d+$")
}

tf_page_number <- function(suffix) {
  if (is.na(suffix)) return(NA_integer_)
  as.integer(stringr::str_extract(suffix, "\\d+"))
}

empty_report_tex_paths <- function() {
  dplyr::tibble(
    type = character(),
    path = character()
  )
}

empty_tf_tbl <- function() {
  dplyr::tibble(
    page = integer(),
    type = character(),
    path = character(),
    code_path = character()
  )
}
