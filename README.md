# Getting Started with review

**review** is a toolkit for ensuring critical files are reviewed on a project. It manages the quality control (QC) process by tracking the assignment and acceptance of files in a centralized log file, **QClog.csv**. **review** is tailored to Subversion (SVN)-based workflows and contains a slightly different log structure.

By integrating with SVN, **review** uses file revisions to detect when a file has been modified and flags it for re-evaluation. It provides functions to assign reviewers, check outstanding QC needs, and summarize the QC status of all files.

The core functions of **review** are:

- **logCreate()**: Creates the QC log.  
- **logAssign()**: Assigns files to reviewers for QC.  
- **logAccept()**: Marks files as reviewed and accepted.  
- **logPending()**: Lists files that still need QC.  
- **logSummary()**: Provides a high-level summary of the QC status of all files.

---

# Core Functions

## logCreate()

To create a QC log, use `logCreate()`. This will generate the **QClog.csv** file at the top level of your project (or at the directory you specify).

```r
library(review)
logCreate()
```

Under the hood, **review** creates an empty **QClog.csv** with the following columns:

| file     | origin   | revf | revo | reviewer | time |
|----------|---------|------|------|----------|------|

- **file**: Name of the file under review.
- **origin**: Name of the file from which this file originates (by default, this is the file itself).
- **revf**: Final revision of the file when assigned or accepted.
- **revo**: Original revision of the file when assigned or accepted.
- **reviewer**: Who is assigned to review the file. 
- **time**: Timestamp of the QC action.

### Example Log (Pre-existing)

For demonstration, assume **QClog.csv** already contains some entries.

| file                      | origin                    | revf | revo | reviewer   | time                |
|---------------------------|---------------------------|------|------|------------|---------------------|
| script/analysis.R         | script/analysis.R         | 12   | 12   | Jane Doe   | 2025-01-06 10:00:00 |
| script/data-prep.R        | script/data-prep.R        | 15   | 15   | John Smith | 2025-01-06 11:30:00 |
| script/visualize.R        | script/visualize.R        | 20   | 20   | Bob Miller | 2025-01-06 13:00:00 |

## logAssign()

Suppose we have authored `script/model-fitting.R` and it is ready for review. We can assign it to Alice Johnson using `logAssign()`. Below is the code for this operation and the updated log.

```r
logAssign(
  file     = "script/model-fitting.R",
  reviewer = "Alice Johnson"
)
```

| file                      | origin                    | revf | revo | reviewer       | time                |
|---------------------------|---------------------------|------|------|----------------|---------------------|
| script/analysis.R         | script/analysis.R         | 12   | 12   | Jane Doe       | 2025-01-06 10:00:00 |
| script/data-prep.R        | script/data-prep.R        | 15   | 15   | John Smith     | 2025-01-06 11:30:00 |
| script/visualize.R        | script/visualize.R        | 20   | 20   | Bob Miller     | 2025-01-06 13:00:00 |
| **script/model-fitting.R** | **script/model-fitting.R** | **26** | **25** | **Alice Johnson** | **2025-01-06 14:00:00** |

> **Note**: The columns **revf** and **revo** are set to the revision at assignment time. These help track whether further revisions occur after initial assignment.

## logAccept()

Once Alice Johnson reviews `script/model-fitting.R`, she can accept it by using `logAccept()`.

```r
logAccept(file = "script/model-fitting.R")
```

This logs the acceptance in **QClog.csv**, potentially with an updated revision if changes were made:

| file                      | origin                    | revf | revo | reviewer       | time                |
|---------------------------|---------------------------|------|------|----------------|---------------------|
| script/analysis.R         | script/analysis.R         | 12   | 12   | Jane Doe       | 2025-01-06 10:00:00 |
| script/data-prep.R        | script/data-prep.R        | 15   | 15   | John Smith     | 2025-01-06 11:30:00 |
| script/visualize.R        | script/visualize.R        | 20   | 20   | Bob Miller     | 2025-01-06 13:00:00 |
| script/model-fitting.R    | script/model-fitting.R    | **26** | **25** | Alice Johnson  | 2025-01-06 14:00:00 |

> **Note**: Here, `revf=26` indicates that the file might have been modified and committed again before final acceptance.

## logPending()

`logPending()` returns files that still require QC. A file is considered pending if it is:

1. Assigned but not yet reviewed or accepted.  
2. Modified (i.e., has a newer SVN revision) since its last QC entry.

An example output of `logPending()` might look like this:

| file               | origin            | revf | headf | revo | heado |
|--------------------|-------------------|------|-------|------|-------|
| script/analysis.R  | script/analysis.R | 12   | 26    | 12   | 26    |

This indicates `script/analysis.R` was modified (or never finalized) and still needs to be accepted because both the file and its origin have newer revisions.

## logSummary()

If we want to see the QC status of all files in the log, we can use `logSummary()` to do so. It aggregates the log entries and shows the latest revision information for each file.

An example output of `logSummary()` might look like this:

| file                   | origin                    | revf | headf | revo | heado | reviewer       | time                |
|------------------------|---------------------------|------|-------|------|-------|----------------|---------------------|
| script/analysis.R      |                           | 12   | 26    | 12   | 26    | Jane Doe       | 2025-01-06 10:00:00 |
| script/data-prep.R     |                           | 15   | 15    | 15   | 15    | John Smith     | 2025-01-06 11:30:00 |
| script/model-fitting.R |                           | 26   | 26    | 25   | 25    | Alice Johnson  | 2025-01-06 14:00:00 |
| script/visualize.R     |                           | 20   | 20    | 20   | 20    | Bob Miller     | 2025-01-06 13:00:00 |

---

# Workflow Example

1. **Create the log**:

    ```r
    logCreate()
    ```

2. **Assign files for review**:

    ```r
    logAssign(file = "script/model-fitting.R", reviewer = "Alice Johnson")
    ```

3. **Mark files as accepted after review**:

    ```r
    logAccept(file = "script/model-fitting.R")
    ```

4. **Display files with outstanding QC**:

    ```r
    logPending()
    ```

5. **View the QC status of all files in the QC log**:

    ```r
    logSummary()
    ```

---


**review** helps ensure all critical files are reviewed in SVN-based projects, giving your team clarity on the QC state of every file and making sure nothing slips through the cracks.

### Cheat Sheet

<a href="https://metrumresearchgroup.github.io/cheatsheets/review_cheat_sheet.pdf"><img src="https://metrumresearchgroup.github.io/cheatsheets/thumbnails/review_cheat_sheet_thumbnail.png" width="200" height="200"/></a>

