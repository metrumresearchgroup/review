# ---- Helpers ----
# Create a PDF with one or more pages of text lines.
# lines_by_page: list where each element is a character vector of lines for that page
create_pdf_with_text <- function(file, lines_by_page) {
  # Basic PDF canvas
  grDevices::pdf(file = file, width = 8.5, height = 11, family = "sans")
  on.exit(grDevices::dev.off(), add = TRUE)
  
  op <- par(mar = c(0, 0, 0, 0)) # no margins
  on.exit(par(op), add = TRUE)
  
  for (lines in lines_by_page) {
    plot.new()
    # Draw each line top-to-bottom; spacing = 0.05 of device height (tweak if needed)
    if (length(lines)) {
      y_positions <- seq(0.95, 0.95 - 0.05 * (length(lines) - 1), by = -0.05)
      for (i in seq_along(lines)) {
        text(x = 0.05, y = y_positions[i], labels = lines[i], adj = c(0, 1))
      }
    }
  }
}

# ---- Tests ----

test_that("getTFLs extracts both figures and tables from a real PDF (single page)", {
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  
  page1 <- c(
    "Some intro text",
    "Source graphic: fig1.png",
    "More filler text",
    "Source file: table1.csv",
    "Source graphic: fig2.png"
  )
  
  create_pdf_with_text(pdf_file, list(page1))
  
  result <- getTFLs(pdf_file)
  
  expect_setequal(
    result,
    c("Source graphic: fig1.png", "Source graphic: fig2.png", "Source file: table1.csv")
  )
  expect_length(result, 3)
})

test_that("getTFLs deduplicates identical entries from a real PDF", {
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  
  page1 <- c(
    "Source file: data1.csv",
    "Source file: data1.csv",
    "Source graphic: figA.png",
    "Source graphic: figA.png"
  )
  
  create_pdf_with_text(pdf_file, list(page1))
  
  result <- getTFLs(pdf_file)
  expect_setequal(result, c("Source file: data1.csv", "Source graphic: figA.png"))
  expect_equal(length(result), 2)
})

test_that("getTFLs returns empty character(0) when no matching lines exist", {
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  
  page1 <- c("No source lines here", "Just regular text")
  create_pdf_with_text(pdf_file, list(page1))
  
  result <- getTFLs(pdf_file)
  expect_length(result, 0)
  expect_true(is.character(result))
})

test_that("getTFLs works when only figures or only tables are present", {
  # Only figures
  pdf_figs <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_figs), add = TRUE)
  create_pdf_with_text(pdf_figs, list(c(
    "Source graphic: only_fig1.png",
    "Source graphic: only_fig2.png"
  )))
  res_figs <- getTFLs(pdf_figs)
  expect_length(res_figs, 2)
  expect_true(all(grepl("^Source graphic:", res_figs)))
  
  # Only tables
  pdf_tabs <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_tabs), add = TRUE)
  create_pdf_with_text(pdf_tabs, list(c(
    "Source file: only_tab1.csv",
    "Source file: only_tab2.csv"
  )))
  res_tabs <- getTFLs(pdf_tabs)
  expect_length(res_tabs, 2)
  expect_true(all(grepl("^Source file:", res_tabs)))
})

test_that("getTFLs collects sources across multiple pages", {
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  
  page1 <- c("Intro", "Source graphic: front_fig.png")
  page2 <- c("Methods", "Source file: data_methods.csv")
  page3 <- c("Results", "Source graphic: res_fig.png", "Source file: res_table.csv")
  
  create_pdf_with_text(pdf_file, list(page1, page2, page3))
  
  result <- getTFLs(pdf_file)
  
  expect_setequal(
    result,
    c(
      "Source graphic: front_fig.png",
      "Source graphic: res_fig.png",
      "Source file: data_methods.csv",
      "Source file: res_table.csv"
    )
  )
  expect_equal(length(result), 4)
})

test_that("getTFLs preserves the intended figure-then-table uniqueness order", {
  # Function returns c(unique(figures), unique(tables))
  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)
  
  page1 <- c(
    "Source file: t1.csv",
    "Source graphic: f1.png",
    "Source file: t2.csv",
    "Source graphic: f2.png"
  )
  create_pdf_with_text(pdf_file, list(page1))
  
  result <- getTFLs(pdf_file)
  
  # Check that all figures appear before tables in the combined vector
  fig_idx <- which(grepl("^Source graphic:", result))
  tab_idx <- which(grepl("^Source file:", result))
  expect_true(all(fig_idx < min(tab_idx)))
})
