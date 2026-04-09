# =============================================================================
# Script    : test_ds_domain.R
# Purpose   : Test suite for the DS domain creation script.
#             Validates the output dataset against SDTM DS domain
#             requirements using the {testthat} framework.
#
# Run AFTER : 01_create_ds_domain.R (which produces ds_domain.rds)
#
# Usage     : source("test_ds_domain.R")
#
# Author    : Ojaswi Bhimineni
# =============================================================================

library(testthat)
library(dplyr)

cat("=============================================================\n")
cat("  DS Domain Test Suite\n")
cat(paste0("  Run Timestamp: ", Sys.time(), "\n"))
cat("=============================================================\n\n")

# -----------------------------------------------------------------------------
# Load output dataset
# -----------------------------------------------------------------------------
if (!file.exists("ds_domain.rds")) {
  stop("ds_domain.rds not found. Please run 01_create_ds_domain.R first.")
}

ds <- readRDS("ds_domain.rds")

# Reload study_ct for CT validation
study_ct <- data.frame(
  stringsAsFactors = FALSE,
  term_value = c(
    "ADVERSE EVENT", "COMPLETED", "DEATH", "LACK OF EFFICACY",
    "LOST TO FOLLOW-UP", "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
    "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR", "WITHDRAWAL BY SUBJECT"
  ),
  collected_value = c(
    "Adverse Event", "Complete", "Dead", "Lack of Efficacy",
    "Lost To Follow-Up", "Physician Decision", "Protocol Violation",
    "Trial Screen Failure", "Study Terminated By Sponsor",
    "Withdrawal by Subject"
  )
)

cat("Running tests...\n\n")


# =============================================================================
# TEST GROUP 1: Structure
# =============================================================================

test_that("Dataset is a data frame", {
  expect_true(is.data.frame(ds))
})

test_that("Dataset has rows", {
  expect_gt(nrow(ds), 0)
})

test_that("All 12 required SDTM DS variables are present", {
  required_vars <- c(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ",
    "DSTERM", "DSDECOD", "DSCAT",
    "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )
  missing <- setdiff(required_vars, names(ds))
  expect_equal(length(missing), 0,
               info = paste("Missing vars:", paste(missing, collapse = ", ")))
})

test_that("Dataset has exactly 12 columns", {
  expect_equal(ncol(ds), 12)
})


# =============================================================================
# TEST GROUP 2: Identifier Variables
# =============================================================================

test_that("DOMAIN is 'DS' for all records", {
  expect_true(all(ds$DOMAIN == "DS"))
})

test_that("STUDYID has no missing values", {
  expect_equal(sum(is.na(ds$STUDYID)), 0)
})

test_that("USUBJID has no missing values", {
  expect_equal(sum(is.na(ds$USUBJID)), 0)
})

test_that("STUDYID is consistent (single study)", {
  expect_equal(length(unique(ds$STUDYID)), 1)
})


# =============================================================================
# TEST GROUP 3: Sequence Number (DSSEQ)
# =============================================================================

test_that("DSSEQ has no missing values", {
  expect_equal(sum(is.na(ds$DSSEQ)), 0)
})

test_that("DSSEQ is unique within each USUBJID", {
  dupes <- ds %>%
    group_by(USUBJID, DSSEQ) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  expect_equal(nrow(dupes), 0)
})

test_that("DSSEQ starts at 1 for every subject", {
  min_seqs <- ds %>%
    group_by(USUBJID) %>%
    summarise(min_seq = min(DSSEQ), .groups = "drop")
  expect_true(all(min_seqs$min_seq == 1))
})

test_that("Primary key (STUDYID + USUBJID + DSSEQ) is unique", {
  pk_dupes <- ds %>%
    group_by(STUDYID, USUBJID, DSSEQ) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(n > 1)
  expect_equal(nrow(pk_dupes), 0)
})


# =============================================================================
# TEST GROUP 4: Controlled Terminology (DSDECOD)
# =============================================================================

test_that("DSDECOD values that are populated come from controlled terminology", {
  # The study CT (C66727) covers standard disposition terms.
  # "RANDOMIZED" appears in this study's raw data as a valid study-specific
  # disposition term (randomization status) which sdtm.oak maps to uppercase.
  # It is included here as an accepted study-level term alongside the
  # standard C66727 terms.
  valid_terms <- c(
    study_ct$term_value,
    "RANDOMIZED"   # Study-specific term: randomization status record
  )
  invalid <- ds %>%
    filter(!is.na(DSDECOD) & !(DSDECOD %in% valid_terms))
  expect_equal(
    nrow(invalid), 0,
    info = paste("Unexpected DSDECOD values:",
                 paste(unique(invalid$DSDECOD), collapse = ", "))
  )
})

test_that("DSDECOD values are uppercase where populated", {
  non_upper <- ds %>%
    filter(!is.na(DSDECOD) & DSDECOD != toupper(DSDECOD))
  expect_equal(nrow(non_upper), 0)
})

test_that("At least some DSDECOD values are populated", {
  # Confirms CT mapping worked for records that had matching values
  n_populated <- sum(!is.na(ds$DSDECOD))
  expect_true(
    n_populated > 0,
    label = paste0("DSDECOD populated count = ", n_populated,
                   " — CT lookup may have failed if 0")
  )
})

test_that("DSTERM is populated where DSDECOD is populated", {
  # If we have a decoded term, we must also have a verbatim term
  missing_term <- ds %>%
    filter(!is.na(DSDECOD) & is.na(DSTERM))
  expect_equal(
    nrow(missing_term), 0,
    info = paste(nrow(missing_term),
                 "records have DSDECOD but missing DSTERM")
  )
})


# =============================================================================
# TEST GROUP 5: Date Variables
# =============================================================================

test_that("DSSTDTC values that are populated are valid ISO 8601 dates", {
  non_na <- ds %>% filter(!is.na(DSSTDTC))
  parsed <- as.Date(substr(non_na$DSSTDTC, 1, 10), format = "%Y-%m-%d")
  expect_equal(sum(is.na(parsed)), 0,
               info = "Some DSSTDTC cannot be parsed as ISO 8601 dates")
})

test_that("DSDTC values that are populated are valid ISO 8601 dates", {
  non_na <- ds %>% filter(!is.na(DSDTC))
  if (nrow(non_na) > 0) {
    parsed <- as.Date(substr(non_na$DSDTC, 1, 10), format = "%Y-%m-%d")
    expect_equal(sum(is.na(parsed)), 0)
  } else {
    succeed("DSDTC is all NA — skipping format check")
  }
})

test_that("DSSTDY is not equal to 0 (day 0 is invalid in SDTM)", {
  zero_days <- ds %>% filter(!is.na(DSSTDY) & DSSTDY == 0)
  expect_equal(nrow(zero_days), 0)
})

test_that("DSSTDY is numeric", {
  expect_true(is.numeric(ds$DSSTDY) | is.integer(ds$DSSTDY))
})


# =============================================================================
# TEST GROUP 6: Output Files
# =============================================================================

test_that("ds_domain.rds output file exists", {
  expect_true(file.exists("ds_domain.rds"))
})

test_that("ds_domain.csv output file exists", {
  expect_true(file.exists("ds_domain.csv"))
})

test_that("CSV and RDS have same number of rows", {
  csv_data <- read.csv("ds_domain.csv", stringsAsFactors = FALSE)
  expect_equal(nrow(csv_data), nrow(ds))
})

test_that("question_1_log.txt exists", {
  expect_true(file.exists("question_1_log.txt"))
})


# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=============================================================\n")
cat("  TEST SUITE COMPLETE\n")
cat(paste0("  Records  : ", nrow(ds), "\n"))
cat(paste0("  Subjects : ", n_distinct(ds$USUBJID), "\n"))
cat(paste0("  DSDECOD populated : ", sum(!is.na(ds$DSDECOD)),
           " / ", nrow(ds), " records\n"))
cat(paste0("  Finished : ", Sys.time(), "\n"))
cat("=============================================================\n")
