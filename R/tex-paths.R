#' Find figure and table files used by a TeX report
#'
#' Compile the main report with LaTeX and return its figure and table paths in one
#' call. LaTeX resolves macros, conditions, nested includes, imports, image search
#' paths, and extensions. The report must build with an installed LaTeX engine
#' and its required packages and assets.
#'
#' The report directory is the working directory for compilation. Build outputs
#' go to a temporary directory and shell escape is disabled. A `% !TeX program`
#' directive selects the engine; otherwise installed pdfLaTeX, LuaLaTeX, and
#' XeLaTeX engines are tried in that order. Auxiliary files must stabilize within
#' four passes. Failed builds raise an error rather than return a partial list.
#' External asset generators and bibliography tools are not run.
#'
#' Figures are files used by LaTeX graphics commands. Tables are
#' files read inside table/tabular environments, or included files that execute a
#' tabular environment. Tables written directly in the main report have no
#' separate asset path and are omitted. Custom table mechanisms using neither
#' table nor tabular environments cannot be classified as tables. Low-level
#' engine image primitives that bypass LaTeX graphics commands are not traced.
#'
#' @param .file Path to the main TeX report.
#' @return A list with `figures` and `tables` character vectors. Paths are unique
#'   and normalized, relative to the most specific containing directory among
#'   the working directory and `here::here()`, or absolute when neither contains
#'   them. Empty categories are returned as `character()`.
#' @examples
#' \dontrun{
#'   assets <- getAssetPathsFromTex("deliv/report/report.tex")
#'   assets$figures
#'   assets$tables
#' }
#' @export
getAssetPathsFromTex <- function(.file) {
  lapply(tex_compile_paths(.file), tex_normalize_paths)
}

tex_normalize_paths <- function(paths) {
  roots <- unique(normalizePath(c(getwd(), here::here()),
    winslash = "/", mustWork = FALSE))
  vapply(unique(paths), function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    containing <- roots[startsWith(path, paste0(roots, "/")) | path == roots]
    if (!length(containing)) return(path)
    root <- containing[[which.max(nchar(containing))]]
    as.character(fs::path_rel(path, root))
  }, character(1), USE.NAMES = FALSE)
}
