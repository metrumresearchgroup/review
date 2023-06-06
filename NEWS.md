# review 3.3.0 development

- `diffQced()` allows the file path to be wrapped in here::here. (#20)

- `diffQced()` now includes arguments for diff mode and ignore white space. (#20)

- `dirPending()` added to package to check if files in a directory are in QC log.

# review 3.2.0

## New features and changes

- `diffFiles()` allows the user to compare the differences between two local files. (#14)

# review 3.1.0

## New features and changes

- `logAssign()` and `logAccept()` now resolve file paths before adding records
  to the QC log. Absolute paths can now be used as well. (#9)

## Bug fixes

- `logPending()` returns multiple outputs for the same file with different
  file paths in the QClog. `script/file.txt` is viewed as a different file
  than `data/../script/file.txt`. Now returns only resolved paths. (#9)

# review 3.0.1

## Bug fixes

- `diffQced()` and `diffPreviousRevisions()` now default to side by side view. (#4)

# review 3.0.0

## New features and changes

- Functions were carried over from the previous `review` package version 2.5, 
  including: `logAccept()`, `logAssign()`, `logCreate()`, `logPending()` and
  `logSummary()`. (#1)
  
- `diffQced()` and `diffPreviousRevisions()` allow the user to view the differences
  between versions of files. `diffQced()` determines the difference using the
  local copy of the file and the most recently QCed version. (#1)
  
- `getQcedRevision()` returns the revision number for a file that's been added to
  the QC log. (#1)
