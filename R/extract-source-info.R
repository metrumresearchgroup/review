#' Extract source metadata embedded at the bottom of an image
#'
#' @param image_path `character(1)` path to an image file, or an image object
#'   accepted by `magick::image_read()`.
#' @param crop_fraction `numeric(1)` fraction of the image height to crop from
#'   the bottom before OCR.
#' @param source_root `character()` optional roots for resolving source-code
#'   paths extracted by OCR.
#' @param tex_graphics `character()` optional figure paths from the report TeX
#'   used to confirm the extracted `Source_Graphic` against ground truth.
#'
#' @return Named list with `Source_Code` and `Source_Graphic` values.
#' @importFrom magick image_crop image_info image_read image_scale
#' @importFrom tesseract ocr ocr_data tesseract
#' @noRd
extract_source_info <- function(
  image_path,
  crop_fraction = 0.20,
  line_ocr = TRUE,
  source_root = source_info_roots(image_path),
  tex_graphics = character(),
  source_files = NULL
) {
  image_name <- if (is.character(image_path) && length(image_path) == 1L) {
    basename(image_path)
  } else {
    NULL
  }
  img <- if (inherits(image_path, "magick-image")) {
    image_path
  } else {
    image_read(image_path)
  }
  info <- image_info(img)

  crop_height <- round(info$height * crop_fraction)
  crop_y_offset <- info$height - crop_height
  img_bottom <- image_crop(
    img,
    paste0(info$width, "x", crop_height, "+0+", crop_y_offset)
  )

  roots <- normalize_source_roots(source_root)
  if (is.null(source_files) && length(roots)) {
    source_files <- source_files_under_roots(roots)
  }

  use_confidence <- isTRUE(line_ocr) || !is.null(image_name)
  candidates <- extract_source_info_candidates(
    img_bottom,
    full_img = img,
    line_ocr = line_ocr,
    use_confidence = use_confidence,
    image_name = image_name,
    source_root = source_root,
    tex_graphics = tex_graphics,
    source_files = source_files
  )
  source_graphic <- choose_source_info_candidate(
    candidates,
    "Source_Graphic",
    image_name = image_name
  )
  source_code <- choose_source_info_candidate(
    candidates,
    "Source_Code",
    source_graphic = source_graphic,
    source_root = source_root,
    source_files = source_files
  )

  list(
    Source_Code = source_code,
    Source_Graphic = source_graphic
  )
}

# Build OCR candidates with confirmation-driven escalation. OCR is only a noisy
# reconstruction; the trustworthy answer comes from anchoring it against ground
# truth -- the source-code path against files on disk, and the graphic path
# against the report TeX. We run the cheap native-scale pass first and stop as
# soon as both fields are confirmed against their anchor. Only fields that are
# still unconfirmed drive the more expensive passes (200% upscale, then the
# dedicated line crop), so the error-prone pages get maximum redundancy while
# cleanly-anchored pages do a single pass.
extract_source_info_candidates <- function(
  img,
  full_img,
  line_ocr = TRUE,
  use_confidence = TRUE,
  image_name = NULL,
  source_root = NULL,
  tex_graphics = character(),
  source_files = NULL
) {
  block_engine <- tesseract(options = list(tessedit_pageseg_mode = 6))

  # Ordered passes, cheapest first. The 200% upscale and the dedicated line crop
  # only run if the native pass left a field unconfirmed.
  passes <- list(
    function() ocr_source_info(img, block_engine, "source_block", use_confidence)
  )
  if (isTRUE(use_confidence)) {
    passes <- c(passes, list(
      function() ocr_source_info(
        image_scale(img, "200%"), block_engine, "source_block", use_confidence
      )
    ))
  }
  if (isTRUE(line_ocr)) {
    line_engine <- tesseract(options = list(tessedit_pageseg_mode = 7))
    passes <- c(passes, list(
      function() ocr_source_info(
        source_code_line(full_img),
        line_engine,
        "source_code_line",
        use_confidence
      )
    ))
  }

  candidates <- NULL
  for (run_pass in passes) {
    candidates <- dplyr::bind_rows(candidates, run_pass())
    if (source_info_confirmed(candidates, image_name, source_root, tex_graphics, source_files)) {
      break
    }
  }

  candidates
}

# Whether the accumulated candidates resolve both fields against ground truth,
# so further OCR passes cannot improve the result. The graphic is confirmed when
# its extracted value matches a report TeX figure path; the source code is
# confirmed when its chosen value resolves to a file on disk. A field with no
# anchor available (no TeX, no source roots) cannot be confirmed and therefore
# always escalates -- exactly the case that needs maximum OCR redundancy.
source_info_confirmed <- function(candidates, image_name, source_root, tex_graphics, source_files = NULL) {
  graphic <- choose_source_info_candidate(
    candidates,
    "Source_Graphic",
    image_name = image_name
  )
  if (!source_graphic_confirmed(graphic, tex_graphics)) {
    return(FALSE)
  }

  code <- choose_source_info_candidate(
    candidates,
    "Source_Code",
    source_graphic = graphic,
    source_root = source_root,
    source_files = source_files
  )
  source_code_confirmed(code, source_root, source_files)
}

# A graphic is confirmed if it closely matches a known TeX figure path.
source_graphic_confirmed <- function(graphic, tex_graphics) {
  if (is.na(graphic) || !length(tex_graphics)) {
    return(FALSE)
  }
  match <- closest_path_match(
    tf_base_path(graphic),
    tex_graphics,
    same_ext = TRUE
  )
  !is.na(match$path)
}

# Source code is confirmed if the chosen value resolves to a file on disk.
source_code_confirmed <- function(code, source_root, source_files = NULL) {
  if (is.na(code) || !is_source_code_path(code)) {
    return(FALSE)
  }
  roots <- normalize_source_roots(source_root)
  if (!length(roots)) {
    return(FALSE)
  }
  files <- if (!is.null(source_files)) source_files else source_files_under_roots(roots)
  resolved <- resolve_source_code_candidate(code, roots, files)
  !is.na(resolved$path) && resolved$distance == 0
}

ocr_source_info <- function(
  img,
  engine,
  candidate_type,
  use_confidence = TRUE
) {
  extracted_text <- ocr(img, engine = engine)
  extracted_data <- if (isTRUE(use_confidence)) {
    ocr_data(img, engine = engine)
  } else {
    NULL
  }
  source_code <- stringr::str_extract(
    extracted_text,
    "(?i)(?<=Source code:\\s).*"
  )
  source_graphic <- stringr::str_extract(
    extracted_text,
    "(?i)(?<=Source graphic:\\s).*"
  )

  source_code <- clean_source_info_value(source_code)
  source_graphic <- clean_source_info_value(source_graphic)

  dplyr::tibble(
    Candidate_Type = candidate_type,
    Source_Code = source_code,
    Source_Code_Confidence = source_info_confidence(
      extracted_data,
      "\\.R(md)?$"
    ),
    Source_Graphic = source_graphic,
    Source_Graphic_Confidence = source_info_confidence(
      extracted_data,
      "\\.(png|pdf)$"
    )
  )
}

source_code_line <- function(img) {
  info <- image_info(img)
  bottom_height <- round(info$height * 0.10)
  bottom <- image_crop(
    img,
    paste0(info$width, "x", bottom_height, "+0+", info$height - bottom_height)
  )
  line <- image_crop(
    bottom,
    paste0(info$width, "x", round(bottom_height * 0.35), "+0+", round(bottom_height * 0.30))
  )
  image_scale(line, "300%")
}

source_info_confidence <- function(extracted_data, pattern) {
  if (is.null(extracted_data) || !nrow(extracted_data)) {
    return(NA_real_)
  }

  words <- clean_source_info_value(extracted_data$word)
  matched <- grepl(pattern, words, ignore.case = TRUE)
  if (!any(matched)) {
    return(NA_real_)
  }

  max(extracted_data$confidence[matched], na.rm = TRUE)
}

choose_source_info_candidate <- function(
  candidates,
  field,
  image_name = NULL,
  source_graphic = NULL,
  source_root = NULL,
  source_files = NULL
) {
  confidence <- candidates[[paste0(field, "_Confidence")]]
  value <- candidates[[field]]
  keep <- !is.na(value)

  if (!any(keep)) {
    return(NA_character_)
  }

  value <- value[keep]
  confidence <- confidence[keep]
  candidate_type <- candidates$Candidate_Type[keep]
  confidence[is.na(confidence)] <- -Inf

  if (identical(field, "Source_Code")) {
    return(choose_source_code_candidate(
      value,
      confidence,
      candidate_type,
      source_graphic,
      source_root,
      source_files
    ))
  }

  if (identical(field, "Source_Graphic") && !is.null(image_name)) {
    basenames <- basename(stringr::str_remove(value, "(?i)\\s+page:\\s+\\d+$"))
    distance <- utils::adist(basenames, image_name)
    best_distance <- min(distance)
    close <- distance == best_distance
    value <- value[close]
    confidence <- confidence[close]
  }

  value[which.max(confidence)]
}

choose_source_code_candidate <- function(
  value,
  confidence,
  candidate_type,
  source_graphic = NULL,
  source_root = NULL,
  source_files = NULL
) {
  valid <- is_source_code_path(value)
  if (!any(valid)) {
    return(NA_character_)
  }

  value <- value[valid]
  confidence <- confidence[valid]
  candidate_type <- candidate_type[valid]

  score <- source_code_candidate_score(
    confidence,
    candidate_type,
    source_code_graphic_support(value, source_graphic)
  )
  resolved <- resolve_source_code_candidates(value, source_root, source_files)
  has_file_match <- !is.na(resolved$path)

  if (any(has_file_match)) {
    score[is.na(score)] <- 0
    score[!has_file_match] <- -Inf
    distance <- resolved$distance
    distance[is.na(distance)] <- 0
    score <- score - (distance * 1000)
    return(resolved$path[[which.max(score)]])
  }

  if (all(is.na(score))) {
    return(NA_character_)
  }

  value[[which.max(score)]]
}

source_code_candidate_score <- function(
  confidence,
  candidate_type,
  graphic_support
) {
  score <- confidence
  score[is.infinite(score)] <- 0

  line_candidate <- candidate_type == "source_code_line"
  score[line_candidate & !is.na(score)] <- score[line_candidate & !is.na(score)] + 15

  score[!is.na(score)] <- score[!is.na(score)] + (graphic_support[!is.na(score)] * 60)

  low_confidence <- !is.infinite(confidence) & confidence < 35
  score[low_confidence & graphic_support == 0] <- NA_real_

  score
}

resolve_source_code_candidates <- function(value, source_root = NULL, source_files = NULL) {
  roots <- normalize_source_roots(source_root)
  if (!length(roots)) {
    return(dplyr::tibble(
      path = rep(NA_character_, length(value)),
      distance = rep(NA_real_, length(value))
    ))
  }

  files <- if (!is.null(source_files)) source_files else source_files_under_roots(roots)
  resolved <- purrr::map(
    value,
    resolve_source_code_candidate,
    roots = roots,
    source_files = files
  )

  dplyr::tibble(
    path = purrr::map_chr(resolved, "path"),
    distance = purrr::map_dbl(resolved, "distance")
  )
}

resolve_source_code_candidate <- function(value, roots, source_files) {
  exact <- existing_source_path(value, roots)
  if (!is.na(exact)) {
    return(list(path = exact, distance = 0))
  }

  closest_existing_source_path(value, roots, source_files)
}

normalize_source_roots <- function(source_root = NULL) {
  if (is.null(source_root)) {
    return(character())
  }

  source_root <- source_root[!is.na(source_root) & nzchar(source_root)]
  source_root <- unique(normalizePath(source_root, winslash = "/", mustWork = FALSE))
  source_root[dir.exists(source_root)]
}

existing_source_path <- function(value, roots) {
  if (!is_source_code_path(value)) {
    return(NA_character_)
  }

  paths <- if (source_path_is_absolute(value)) {
    value
  } else {
    file.path(roots, value)
  }
  exists <- file.exists(paths)
  if (!any(exists)) {
    return(NA_character_)
  }

  source_relative_path(paths[which(exists)[[1]]], roots)
}

source_path_is_absolute <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

closest_existing_source_path <- function(value, roots, files = source_files_under_roots(roots)) {
  if (!length(files)) {
    return(list(path = NA_character_, distance = NA_real_))
  }

  relative_paths <- purrr::map_chr(files, source_relative_path, roots = roots)
  match <- closest_path_match(value, relative_paths, same_ext = TRUE)
  if (is.na(match$path)) {
    return(match)
  }

  list(
    path = if (identical(dirname(value), ".")) basename(match$path) else match$path,
    distance = match$distance
  )
}

source_files_under_roots <- function(roots) {
  files <- purrr::map(
    roots,
    list.files,
    pattern = "\\.R(md)?$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  files <- unlist(files, use.names = FALSE)
  unique(files[file.exists(files)])
}

closest_path_match <- function(path, candidates, same_ext = FALSE) {
  if (is.na(path) || !nzchar(path)) {
    return(list(path = NA_character_, distance = NA_real_))
  }

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (same_ext) {
    candidates <- candidates[
      tolower(tools::file_ext(candidates)) == tolower(tools::file_ext(path))
    ]
  }
  if (!length(candidates)) {
    return(list(path = NA_character_, distance = NA_real_))
  }

  if (path %in% candidates) {
    return(list(path = path, distance = 0))
  }

  basename_distance <- as.numeric(utils::adist(basename(candidates), basename(path)))
  best_distance <- min(basename_distance)
  max_distance <- max(2, ceiling(nchar(basename(path)) * 0.20))
  if (best_distance > max_distance) {
    return(list(path = NA_character_, distance = NA_real_))
  }

  close <- basename_distance == best_distance
  path_distance <- as.numeric(utils::adist(candidates[close], path))
  best <- which(close)[[which.min(path_distance)]]

  list(path = candidates[[best]], distance = best_distance)
}

source_relative_path <- function(path, roots) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  roots <- normalizePath(roots, winslash = "/", mustWork = FALSE)
  roots <- roots[startsWith(path, paste0(roots, "/")) | path == roots]
  if (!length(roots)) {
    return(path)
  }

  root <- roots[[which.max(nchar(roots))]]
  sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", root), "/?"), "", path)
}

source_info_roots <- function(path = NULL) {
  roots <- character()

  if (is.character(path) && length(path) == 1L && nzchar(path)) {
    roots <- c(roots, source_root_from_path(path))
  }

  roots <- c(roots, here::here())

  normalize_source_roots(unique(roots))
}

source_root_from_path <- function(path) {
  current <- dirname(normalizePath(path, winslash = "/", mustWork = FALSE))
  ancestors <- path_ancestors(current)

  marked <- purrr::map_lgl(ancestors, source_root_marker)
  if (any(marked)) {
    return(ancestors[which(marked)[[1]]])
  }

  current
}

path_ancestors <- function(path, max_depth = 8L) {
  ancestors <- path
  current <- path

  for (i in seq_len(max_depth)) {
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    ancestors <- c(ancestors, parent)
    current <- parent
  }

  unique(ancestors)
}

source_root_marker <- function(path) {
  if (!dir.exists(path)) {
    return(FALSE)
  }

  any(dir.exists(file.path(path, c("script", "scripts", "R")))) ||
    any(file.exists(file.path(path, c("renv.lock", "pkgr.yml", ".here")))) ||
    dir.exists(file.path(path, ".git")) ||
    length(list.files(path, pattern = "\\.Rproj$", full.names = TRUE)) > 0
}

source_code_graphic_support <- function(value, source_graphic = NULL) {
  if (is.null(source_graphic) || is.na(source_graphic)) {
    return(rep(0L, length(value)))
  }

  graphic_tokens <- source_info_tokens(basename(source_graphic))
  graphic_tokens <- graphic_tokens[nchar(graphic_tokens) >= 3]
  if (!length(graphic_tokens)) {
    return(rep(0L, length(value)))
  }

  purrr::map_int(value, function(candidate) {
    target <- tolower(basename(candidate))
    sum(purrr::map_lgl(graphic_tokens, stringr::str_detect, string = target))
  })
}

source_info_tokens <- function(value) {
  tokens <- unlist(strsplit(tolower(value), "[^[:alnum:]]+"))
  tokens[nzchar(tokens)]
}

is_source_code_path <- function(path) {
  grepl("^[[:alnum:]_./-]+\\.R(md)?$", path, ignore.case = TRUE)
}

clean_source_info_value <- function(value) {
  if (length(value) == 0L) {
    return(character())
  }

  missing <- is.na(value)
  value <- stringr::str_trim(value)
  value <- stringr::str_replace_all(value, "[\u2013\u2014\u2212]", "-")

  page_suffix <- stringr::str_extract(value, "(?i)\\s+page\\s*:\\s*\\d+\\s*$")
  value <- stringr::str_remove(value, "(?i)\\s+page\\s*:\\s*\\d+\\s*$")
  value <- stringr::str_remove_all(value, "\\s+")
  value <- stringr::str_remove_all(value, "[`'\u2018\u2019]")
  value <- stringr::str_replace(value, "\\.R(md)?[\\.:]+$", ".R\\1")
  value <- stringr::str_replace(value, "[,._]png$", ".png")
  value <- stringr::str_replace_all(value, "/+", "/")
  value <- stringr::str_replace_all(value, "-+", "-")

  has_page_suffix <- !is.na(page_suffix)
  page_number <- stringr::str_extract(page_suffix, "\\d+")
  value[has_page_suffix] <- paste0(
    value[has_page_suffix],
    " page: ",
    page_number[has_page_suffix]
  )

  value[missing] <- NA_character_
  value
}
