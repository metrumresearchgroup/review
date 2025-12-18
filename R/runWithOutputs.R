 #' Run a Script and Report Outputs
 #'
 #' Runs an R script from the project root in a clean R session and reports any
 #' files modified during the run as YAML. Uses `fd` to identify files (excluding
 #' the `renv` directory) and prints the relative paths for downstream tooling.
 #'
#' @param script Path to the script to execute.
#'
#' @return Invisibly returns `NULL`; prints YAML of files changed since the
#'   script started.
#'
#' @examples
#' \dontrun{
#'   runWithOutputs("path/to/script.R")
#' }
#'
#' @export
runWithOutputs <- function(script) {
  root <- here::here()

  # 1. Capture start time to establish a baseline for file modifications
  t0 <- Sys.time()

  # 2. Execute the script in a fresh R session
  callr::rscript(script, wd = root)

  # 3. List candidate files using `fd`
  res <- processx::run(
    "fd",
    args = c("--type", "f", "--exclude", "renv"),
    wd = root
  )

  if (res$stdout != "") {
    files <- unlist(strsplit(trimws(res$stdout), "\n"))
    abs_paths <- fs::path(root, files)
    changed <- files[file.mtime(abs_paths) > t0]

    # 5. Output results as YAML
    if (length(changed) > 0) {
      changed <- sort(changed)
      yaml_str <- paste0(
        "outputs:\n",
        paste0("   - \"", changed, "\"\n", collapse = "")
      )
      cli::cli_code(yaml_str)
    }
  }
}
