# review 3.0.0

## New features and changes

- Functions were carried over from the previous `review` package version 2.5, 
  including: `logAccept()`, `logAssign()`, `logCreate()`, `logPending()` and
  `logSummary()`.
  
- `diffQced()` and `diffPreviousRevisions()` allow the user to view the differences
  between versions of files. `diffQced()` determines the difference using the
  local copy of the file and the most recently QCed version.
  
- `getQcedRevision()` returns the revision number for a file that's been added to
  the QC log.