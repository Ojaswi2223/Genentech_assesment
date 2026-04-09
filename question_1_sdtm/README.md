# Question 1: SDTM DS Domain Creation using {sdtm.oak}

## Overview

This folder contains the R programming solution for **Question 1** of the
Roche/Genentech ADS Programmer Coding Assessment.

The goal is to create a **CDISC SDTM Disposition (DS) Domain** dataset from
raw clinical trial eCRF data using the **{sdtm.oak}** Pharmaverse package.

---

## Folder Structure

```
question_1_sdtm/
├── 01_create_ds_domain.R    # Main script: DS domain creation
├── test_ds_domain.R         # Test suite: validates output dataset
├── ds_domain.rds            # Output: DS dataset (R native format)
├── ds_domain.csv            # Output: DS dataset (CSV format)
├── question_1_log.txt       # Output: Execution log (error-free evidence)
└── README.md                # This file
```

---

## How to Run

### Step 1 — Install Required Packages
```r
install.packages(c(
  "sdtm.oak",
  "pharmaverseraw",
  "dplyr",
  "lubridate",
  "stringr",
  "tibble",
  "testthat"
))
```

### Step 2 — Set Working Directory
```r
setwd("path/to/question_1_sdtm")
```

### Step 3 — Run the Main Script
```r
source("01_create_ds_domain.R")
```

### Step 4 — Run the Test Suite
```r
source("test_ds_domain.R")
```

---

## What the Script Does

### Input Data
| Source | Description |
|--------|-------------|
| `pharmaverseraw::ds_raw` | Raw disposition eCRF data |
| `study_ct` (defined inline) | Study controlled terminology (NCI codelist C66727) |

### Output Variables

| Variable | Type | Description |
|----------|------|-------------|
| STUDYID | Identifier | Study identifier |
| DOMAIN | Identifier | Always "DS" |
| USUBJID | Identifier | Unique subject identifier |
| DSSEQ | Identifier | Sequence number (unique per subject) |
| DSTERM | Topic | Verbatim disposition term from eCRF |
| DSDECOD | Qualifier | CDISC-standardized decoded term |
| DSCAT | Qualifier | Category of disposition event |
| VISITNUM | Timing | Visit number |
| VISIT | Timing | Visit name |
| DSDTC | Timing | Date of data collection (ISO 8601) |
| DSSTDTC | Timing | Start date of disposition event (ISO 8601) |
| DSSTDY | Timing | Study day of disposition event |

---

## Key Design Decisions

### 1. {sdtm.oak} Functions Used
- **`assign_no_ct()`** — Direct variable mapping from raw to SDTM (no CT lookup needed)
- **`assign_ct()`** — Maps raw collected values to CDISC standard terms via study_ct
- **`hardcode_no_ct()`** — Assigns a fixed value (e.g., DOMAIN = "DS")

### 2. DSDECOD Controlled Terminology Mapping
Raw collected values (e.g., `"Complete"`, `"Dead"`) are mapped to their CDISC
standard decoded terms (e.g., `"COMPLETED"`, `"DEATH"`) using the `study_ct`
controlled terminology table and NCI codelist **C66727**.

### 3. DSSTDY Derivation
DSSTDY is derived as:
```
DSSTDY = DSSTDTC - RFSTDTC + 1  (for dates on/after reference start)
DSSTDY = DSSTDTC - RFSTDTC      (for dates before reference start)
```
Since DM domain is not assembled in this script, the earliest DSSTDTC per
subject is used as a proxy reference start date. In a full submission pipeline,
RFSTDTC would be joined from ADSL/DM.

> **Note:** SDTM study days have no day 0. Day 1 = reference start date,
> day -1 = one day before reference start.

### 4. DSSEQ Derivation
Records are sorted by USUBJID and DSSTDTC, then numbered sequentially
within each subject starting from 1. This ensures the primary key
(STUDYID + USUBJID + DSSEQ) is unique across all records.

### 5. Logging
All console output is captured to `question_1_log.txt` using `sink()`,
providing evidence of error-free execution as required by the assessment.

---

## QC Checks Performed

The script runs 10 inline QC checks and the test suite runs 20 unit tests
covering:

| Category | Checks |
|----------|--------|
| Structure | All 12 required variables present, correct column count |
| Identifiers | No missing STUDYID/USUBJID, DOMAIN = "DS" |
| Sequence | DSSEQ unique, sequential, starts at 1 per subject |
| Controlled Terminology | DSDECOD values from study_ct, uppercase |
| Dates | ISO 8601 format, no day 0 in DSSTDY |
| Integrity | No empty strings, primary key uniqueness |
| Output Files | RDS, CSV, and log file all exist |

---

## CDISC References

- [SDTM Implementation Guide v3.4 — DS Domain](https://www.cdisc.org/standards/foundational/sdtmig)
- [NCI Codelist C66727 — Completion/Reason for Non-Completion](https://evs.nci.nih.gov/)
- [{sdtm.oak} Documentation](https://pharmaverse.github.io/sdtm.oak/)
- [Pharmaverse Examples — SDTM Section](https://pharmaverse.github.io/examples/)

---

- AI coding assistants were used to accelerate development, consistent
  with assessment guidelines
- All derivation logic is inde
