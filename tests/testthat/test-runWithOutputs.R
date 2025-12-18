test_that("runWithOutputs detects newly created files", {
  with_demoRepo({
    # Setup: Create a script that generates a new output file
    script_path <- "script/generate_data.R"
    output_file <- "data/new_result.txt"
    
    # using fs::dir_create handles parent dirs and existence checks automatically
    fs::dir_create("script") 
    
    writeLines(
      text = c(
        'dir.create("data", showWarnings = FALSE)',
        paste0('writeLines("content", "', output_file, '")')
      ),
      con = script_path
    )
    
    # Execution
    result <- runWithOutputs(script_path, root = getwd())
    
    # Verification: Check return value
    expect_true(output_file %in% result)
    # Ensure the script itself is not listed as an output
    expect_false(script_path %in% result)
  })
})

test_that("runWithOutputs detects modified files", {
  with_demoRepo({
    # Setup: Create an existing file
    target_file <- "data/existing_file.txt"
    fs::dir_create("data")
    writeLines("old content", target_file)
    
    # OPTIMIZATION: Manually backdate the file timestamp.
    # This ensures the test is robust without using Sys.sleep().
    Sys.setFileTime(target_file, Sys.time() - 60)
    
    script_path <- "script/modify_data.R"
    fs::dir_create("script")
    
    writeLines(
      text = paste0('writeLines("new content", "', target_file, '")'),
      con = script_path
    )
    
    # Execution
    result <- runWithOutputs(script_path, root = getwd())
    
    # Verification
    expect_true(target_file %in% result)
  })
})

test_that("runWithOutputs returns empty vector when no files change", {
  with_demoRepo({
    script_path <- "script/do_nothing.R"
    fs::dir_create("script")
    writeLines("print('No file changes here')", script_path)
    
    # Execution
    result <- runWithOutputs(script_path, root = getwd())
    
    expect_identical(result, character(0))
  })
})

test_that("runWithOutputs respects default excluded directories", {
  with_demoRepo({
    # Setup: Create directories
    fs::dir_create("renv")
    fs::dir_create("scratch")
    fs::dir_create("data")
    fs::dir_create("script")
    
    script_path <- "script/mixed_outputs.R"
    
    # Script writes to:
    # 1. renv (should be ignored by DEFAULT exclusion)
    # 2. scratch (should be included, as it's not a default exclusion)
    # 3. data (should be included)
    writeLines(
      text = c(
        'writeLines("a", "renv/ignored.txt")',
        'writeLines("b", "scratch/included.txt")',
        'writeLines("c", "data/important.txt")'
      ),
      con = script_path
    )
    
    # Execution
    result <- runWithOutputs(script_path, root = getwd())
    
    # Verification
    expect_true("data/important.txt" %in% result)
    expect_true("scratch/included.txt" %in% result)
    expect_false("renv/ignored.txt" %in% result)
  })
})

test_that("runWithOutputs respects custom excluded directories argument", {
  with_demoRepo({
    fs::dir_create("custom_folder")
    script_path <- "script.R"
    
    writeLines('writeLines("x", "custom_folder/file.txt")', script_path)
    
    # Pass a custom exclusion list
    result <- runWithOutputs(
      script_path, 
      root = getwd(), 
      exclude_dirs = c("custom_folder")
    )
    
    expect_false("custom_folder/file.txt" %in% result)
  })
})

test_that("runWithOutputs errors if the script execution fails", {
  with_demoRepo({
    script_path <- "broken.R"
    writeLines("stop('Critical error')", script_path)
    
    # Verification
    expect_error(runWithOutputs(script_path, root = getwd()))
  })
})