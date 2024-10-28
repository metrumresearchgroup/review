# review development

## New features and changes

- `fileRename` added to the package to assist with renaming files in SVN and QClog. (#104)

- `diffFiles` can now display the entire file that is being diffed. (#115)

- Added `repoHistory` function to return history of all commits in the repository. (#116)

# review 3.9.1

## New features and changes

- `compareFigures` and `compareTables` now have hide on open option. (#97)

## Bug fixes

- Improved run time of `dirSummary` when used on larger repositories. (#98)

# review 3.9.0

## New features and changes

- `compareFigures` and `compareTables` added as a comparison tool for figures and tables. (#94)

- `compareModified` and `compareLocal` switched to internal functions. (#94)

- `compareModified` and `compareLocal` now have a file extension argument. (#92)

# review 3.8.2

## New features and changes

- `compareLocal` allows comparison between local versions of figures or tables. (#88)

## Bug fixes

- Allow spaces in files when using `compareModified`. (#88)

# review 3.8.1

## Bug fixes

- Fixed directory counts displayed by `dirSummary`. (#86)

# review 3.8.0

## New features and changes

- `dirSummary` and `renderQCSummary` now provide a `.dirs_exclude` option. (#80)

- Added `compareModified` function to show comparisons between different versions of figures or tables. (#78)

- Update `svnExport` to automatically use the last checked in version of a file if previous revision isn't provided. (#78)

- Added `with_demoRepo` function to allow the user easier access to running examples in the demo Repo. (#77)

- Added warning message to `diffQCed` when user has modified file since last QC. (#61)

- Added `svnProjInfo` function to determine svn project info. (#72)

# review 3.7.0

## New features and changes 

- Added vignettes for the SVN helper functions and general QC helpers. (#54, #55)

- Added `demoRepo` function to create an example SVN repository with existing files/QC. (#51)

- Improved run time of `renderQCSummary` with new internal `svnInfo` function. (#49, #50)

# review 3.6.0

## New features and changes

- `svnExport` created function to run svn export in R. (#39)

## Bug fixes

- `diffQced` has more informative error message when no previous QC revision is found (#44)

# review 3.5.0

## New features and changes

- `dirSummary` added to provide QC status of all scripts/models in a given directory. (#29, #30)

- `renderQCSummary` added to create a pdf summary of the QC status of scripts in a given directory. (#29, #31, #32, #33, #35, #36)

# review 3.4.0

## New features and changes

- `svnLog` added to package to return the svn log in dataframe format. (#25)

# review 3.3.0 

- `diffQced()` allows the file path to be wrapped in here::here. (#20)

- `diffQced()` now includes arguments for diff mode and ignore white space. (#20)

- `dirPending()` added to package to check if files in a directory are in QC log.(#21)

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
