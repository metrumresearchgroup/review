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