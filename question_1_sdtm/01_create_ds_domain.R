# =============================================================================
# Script    : 01_create_ds_domain.R
# Purpose   : Create SDTM Disposition (DS) Domain dataset from raw clinical
#             trial data using the {sdtm.oak} Pharmaverse package.
#
# Assessment: Roche/Genentech ADS Programmer Coding Assessment - Question 1
#
# Author    : Ojaswi Bhimineni
# Date      : 04/09/2026
#
# Inputs    :
#   - pharmaverseraw::ds_raw   : Raw disposition eCRF data (850 rows, 13 cols)
#   - study_ct                 : Study controlled terminology (defined inline)
#
# Raw Data Columns:
#   STUDY, PATNUM, SITENM, INSTANCE, FORM, FORML,
#   IT.DSTERM, IT.DSDECOD, OTHERSP, DSDTCOL, DSTMCOL, IT.DSSTDAT, DEATHDT
#
# Outputs:
#   - ds_domain.rds         : Final DS domain (R format)
#   - ds_domain.csv         : Final DS domain (CSV format)
#   - question_1_log.txt    : Execution log (error-free evidence)
#
# Required Output Variables:
#   STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
#   DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
#
# CDISC Reference:
#   SDTM Implementation Guide v3.4 - DS Domain
#   https://www.cdisc.org/standards/foundational/sdtmig
#
# Key {sdtm.oak} functions used:
#   generate_oak_id_vars() : Adds required oak_id, raw_source, patient_number
#   oak_id_vars()          : Returns the vector of oak identifier variable names
#   assign_no_ct()         : Maps raw variable directly to SDTM (no CT lookup)
#   assign_ct()            : Maps raw variable via controlled terminology
#   hardcode_no_ct()       : Assigns a fixed hardcoded value to all records
#   derive_seq()           : Derives the sequence number (DSSEQ)
#   derive_study_day()     : Derives study day (DSSTDY)
# =============================================================================


# =============================================================================
# SECTION 1: Load Required Packages
# =============================================================================

library(sdtm.oak)        # Pharmaverse SDTM programming package
library(pharmaverseraw)  # Raw clinical trial example datasets
library(dplyr)           # Data manipulation
library(lubridate)       # Date handling


# =============================================================================
# SECTION 2: Configure Logging
# =============================================================================
# All output captured to question_1_log.txt as evidence of error-free run

log_file <- "question_1_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, append = FALSE, type = "output")
sink(log_con, append = TRUE,  type = "message")

cat("=============================================================\n")
cat("  SDTM DS Domain Creation Log\n")
cat("  Roche/Genentech ADS Programmer Assessment - Question 1\n")
cat(paste0("  Run Timestamp: ", Sys.time(), "\n"))
cat("=============================================================\n\n")


# =============================================================================
# SECTION 3: Define Study Controlled Terminology (study_ct)
# =============================================================================
# Maps raw eCRF collected values to CDISC standard decoded terms (DSDECOD).
# NCI Codelist C66727 = Completion/Reason for Non-Completion
#
# Columns:
#   codelist_code    : NCI codelist identifier
#   term_code        : NCI concept code
#   term_value       : CDISC standard decoded value -> used in DSDECOD
#   collected_value  : Raw eCRF value -> used for CT lookup matching
#   term_preferred_term / term_synonyms : Optional mapping aids

cat("STEP 1: Defining Study Controlled Terminology (study_ct)...\n")

study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = rep("C66727", 10),
  term_code = c(
    "C41331",  # ADVERSE EVENT
    "C25250",  # COMPLETED
    "C28554",  # DEATH
    "C48226",  # LACK OF EFFICACY
    "C48227",  # LOST TO FOLLOW-UP
    "C48250",  # PHYSICIAN DECISION
    "C142185", # PROTOCOL VIOLATION
    "C49628",  # SCREEN FAILURE
    "C49632",  # STUDY TERMINATED BY SPONSOR
    "C49634"   # WITHDRAWAL BY SUBJECT
  ),
  term_value = c(
    "ADVERSE EVENT", "COMPLETED", "DEATH",
    "LACK OF EFFICACY", "LOST TO FOLLOW-UP",
    "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
    "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR",
    "WITHDRAWAL BY SUBJECT"
  ),
  collected_value = c(
    "Adverse Event", "Complete", "Dead",
    "Lack of Efficacy", "Lost To Follow-Up",
    "Physician Decision", "Protocol Violation",
    "Trial Screen Failure", "Study Terminated By Sponsor",
    "Withdrawal by Subject"
  ),
  term_preferred_term = c(
    "AE", "Completed", "Died", NA, NA, NA,
    "Violation", "Failure to Meet Inclusion/Exclusion Criteria",
    NA, "Dropout"
  ),
  term_synonyms = c(
    "ADVERSE EVENT", "COMPLETE", "Death",
    NA, NA, NA, NA, NA, NA, "Discontinued Participation"
  )
)

cat(paste0("  Rows  : ", nrow(study_ct), "\n"))
cat(paste0("  Terms : ", paste(study_ct$term_value, collapse = ", "), "\n\n"))


# =============================================================================
# SECTION 4: Load Raw Input Data
# =============================================================================

cat("STEP 2: Loading pharmaverseraw::ds_raw...\n")

ds_raw <- pharmaverseraw::ds_raw

cat(paste0("  Rows   : ", nrow(ds_raw), "\n"))
cat(paste0("  Columns: ", ncol(ds_raw), "\n"))
cat(paste0("  Names  : ", paste(names(ds_raw), collapse = ", "), "\n\n"))
cat("  First 3 rows:\n")
print(head(ds_raw, 3))
cat("\n")


# =============================================================================
# SECTION 5: Add {sdtm.oak} Required Metadata Columns
# =============================================================================
# {sdtm.oak} requires three metadata columns on the raw dataset before
# any mapping function (assign_no_ct, assign_ct, hardcode_no_ct) will work:
#
#   oak_id        : Unique integer row identifier
#   raw_source    : Name of the raw data source
#   patient_number: Subject identifier
#
# generate_oak_id_vars() is the correct {sdtm.oak} function to add these.
# Arguments:
#   pat_var  : Column name containing the patient/subject number
#   raw_src  : String name of the raw data source

cat("STEP 3: Adding sdtm.oak metadata via generate_oak_id_vars()...\n")

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",     # Patient number column in ds_raw
    raw_src = "ds_raw"      # Name of this raw data source
  )

cat(paste0("  oak metadata columns added: ",
           paste(oak_id_vars(), collapse = ", "), "\n"))
cat(paste0("  Rows after metadata: ", nrow(ds_raw), "\n\n"))


# =============================================================================
# SECTION 6: Pre-process Raw Variables
# =============================================================================
# Some raw variables need light preparation before sdtm.oak mapping:
#   - USUBJID construction (STUDY + "-" + PATNUM)
#   - DSTERM fallback logic (IT.DSTERM -> IT.DSDECOD -> OTHERSP)
#   - DSCAT hardcoded category
#   - VISITNUM numeric mapping from INSTANCE visit labels
#   - Date conversion from MM-DD-YYYY to ISO 8601 YYYY-MM-DD

cat("STEP 4: Pre-processing raw variables for SDTM mapping...\n")

# Helper: safely convert MM-DD-YYYY dates to ISO 8601 YYYY-MM-DD
convert_date <- function(x) {
  result <- suppressWarnings(
    format(as.Date(x, format = "%m-%d-%Y"), "%Y-%m-%d")
  )
  ifelse(is.na(result) | result == "NA", NA_character_, result)
}

ds_raw <- ds_raw %>%
  mutate(
    # USUBJID: CDISC standard = STUDYID-PATNUM
    USUBJID_RAW  = paste0(STUDY, "-", PATNUM),

    # DSTERM: verbatim text (fallback chain)
    DSTERM_RAW   = case_when(
      !is.na(IT.DSTERM)  & trimws(IT.DSTERM)  != "" ~ IT.DSTERM,
      !is.na(IT.DSDECOD) & trimws(IT.DSDECOD) != "" ~ IT.DSDECOD,
      !is.na(OTHERSP)    & trimws(OTHERSP)    != "" ~ OTHERSP,
      TRUE ~ NA_character_
    ),

    # DSCAT: all records are disposition events
    DSCAT_RAW    = "DISPOSITION EVENT",

    # VISITNUM: numeric mapping from visit label
    VISITNUM_RAW = case_when(
      INSTANCE == "Baseline"           ~ 1,
      INSTANCE == "Week 2"             ~ 2,
      INSTANCE == "Week 4"             ~ 3,
      INSTANCE == "Week 8"             ~ 4,
      INSTANCE == "Week 12"            ~ 5,
      INSTANCE == "Week 16"            ~ 6,
      INSTANCE == "Week 20"            ~ 7,
      INSTANCE == "Week 24"            ~ 8,
      INSTANCE == "Week 26"            ~ 9,
      INSTANCE == "Early Termination"  ~ 99,
      TRUE                             ~ NA_real_
    ),

    # DSDTC: collection date in ISO 8601
    DSDTC_RAW    = convert_date(DSDTCOL),

    # DSSTDTC: event start date in ISO 8601 (fallback to DEATHDT)
    DSSTDTC_RAW  = case_when(
      !is.na(IT.DSSTDAT) & trimws(IT.DSSTDAT) != "" ~
        convert_date(IT.DSSTDAT),
      !is.na(DEATHDT) & trimws(DEATHDT) != "" ~
        convert_date(DEATHDT),
      TRUE ~ NA_character_
    )
  )

cat("  Pre-processing complete.\n\n")


# =============================================================================
# SECTION 7: Derive SDTM Variables Using {sdtm.oak}
# =============================================================================
# Each variable is derived using the appropriate {sdtm.oak} algorithm.
# All functions require id_vars = oak_id_vars() to link records correctly.
#
# Pattern:
#   ds <- assign_no_ct(raw_dat, raw_var, tgt_var, id_vars = oak_id_vars())
#   ds <- assign_ct(raw_dat, raw_var, tgt_var, ct_spec, ct_clst, id_vars)
#   ds <- hardcode_no_ct(raw_dat, raw_var, tgt_var, tgt_val, id_vars)

cat("STEP 5: Deriving SDTM variables using {sdtm.oak} algorithms...\n\n")

# -----------------------------------------------------------------------------
# STUDYID: Study identifier
# Algorithm : assign_no_ct — direct mapping, no CT lookup
# Source    : STUDY column
# -----------------------------------------------------------------------------
cat("  Deriving STUDYID (assign_no_ct)...\n")
ds <- assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "STUDY",
  tgt_var = "STUDYID",
  id_vars = oak_id_vars()
)

# -----------------------------------------------------------------------------
# DOMAIN: Domain abbreviation
# Algorithm : hardcode_no_ct — fixed value "DS" for all records
# -----------------------------------------------------------------------------
cat("  Deriving DOMAIN (hardcode_no_ct)...\n")
ds <- ds %>%
  hardcode_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",    # raw_var required but value not used (hardcoded)
    tgt_var = "DOMAIN",
    tgt_val = "DS",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# USUBJID: Unique subject identifier
# Algorithm : assign_no_ct — direct mapping from pre-constructed USUBJID_RAW
# Source    : USUBJID_RAW = STUDY + "-" + PATNUM
# -----------------------------------------------------------------------------
cat("  Deriving USUBJID (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "USUBJID_RAW",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# DSTERM: Verbatim reported disposition term
# Algorithm : assign_no_ct — preserves original eCRF text
# Source    : DSTERM_RAW (IT.DSTERM with fallbacks)
# -----------------------------------------------------------------------------
cat("  Deriving DSTERM (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSTERM_RAW",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# DSDECOD: Standardized disposition term
# Algorithm : assign_ct — CT lookup maps collected value to CDISC term
# Source    : IT.DSDECOD -> study_ct -> term_value
# Note      : ct_spec argument is the correct parameter name (not ct_dat)
# -----------------------------------------------------------------------------
cat("  Deriving DSDECOD (assign_ct with study_ct)...\n")
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,       # Controlled terminology specification
    ct_clst = "C66727",       # NCI codelist code
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# DSCAT: Category for disposition event
# Algorithm : hardcode_no_ct — fixed value for all records
# -----------------------------------------------------------------------------
cat("  Deriving DSCAT (hardcode_no_ct)...\n")
ds <- ds %>%
  hardcode_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# VISITNUM: Visit number
# Algorithm : assign_no_ct — direct mapping from pre-mapped numeric values
# Source    : VISITNUM_RAW (mapped from INSTANCE)
# -----------------------------------------------------------------------------
cat("  Deriving VISITNUM (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "VISITNUM_RAW",
    tgt_var = "VISITNUM",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# VISIT: Visit name
# Algorithm : assign_no_ct — direct mapping from INSTANCE
# Source    : INSTANCE (protocol visit label)
# -----------------------------------------------------------------------------
cat("  Deriving VISIT (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# DSDTC: Date of data collection (ISO 8601)
# Algorithm : assign_no_ct — direct mapping from pre-converted date
# Source    : DSDTC_RAW (converted from DSDTCOL MM-DD-YYYY)
# -----------------------------------------------------------------------------
cat("  Deriving DSDTC (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSDTC_RAW",
    tgt_var = "DSDTC",
    id_vars = oak_id_vars()
  )

# -----------------------------------------------------------------------------
# DSSTDTC: Start date of disposition event (ISO 8601)
# Algorithm : assign_no_ct — direct mapping from pre-converted date
# Source    : DSSTDTC_RAW (converted from IT.DSSTDAT, fallback DEATHDT)
# -----------------------------------------------------------------------------
cat("  Deriving DSSTDTC (assign_no_ct)...\n")
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "DSSTDTC_RAW",
    tgt_var = "DSSTDTC",
    id_vars = oak_id_vars()
  )

cat("\n  All SDTM variables derived.\n\n")


# =============================================================================
# SECTION 8: Derive DSSTDY — Study Day
# =============================================================================
# SDTM study day rules (no day 0 exists):
#   Day  1 = reference start date
#   Day -1 = one day before reference start
#
# derive_study_day() is the {sdtm.oak} function for this derivation.
# Since DM is not assembled here, we use the earliest DSSTDTC per subject
# as a proxy reference start date (RFSTDTC).

cat("STEP 6: Deriving DSSTDY using derive_study_day()...\n")

# derive_study_day() correct API (from sdtm.oak CRAN documentation):
#   derive_study_day(sdtm_in, dm_domain, tgdt, refdt, study_day_var,
#                   merge_key = "USUBJID")
#
# Both date columns must be Date type (not character) before calling.
# We build a proxy dm_domain with RFSTDTC = earliest DSSTDTC per subject.

# Step 1: Build proxy reference date dataframe (mimics DM domain)
dm_proxy <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  group_by(USUBJID) %>%
  summarise(
    RFSTDTC = min(as.Date(substr(DSSTDTC, 1, 10), format = "%Y-%m-%d"),
                  na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: Convert DSSTDTC in ds to Date for derive_study_day()
ds <- ds %>%
  mutate(DSSTDTC_DT = as.Date(substr(DSSTDTC, 1, 10), format = "%Y-%m-%d"))

# Step 3: Call derive_study_day() with correct positional arguments
ds <- derive_study_day(
  sdtm_in       = ds,           # SDTM dataset containing target date
  dm_domain     = dm_proxy,     # Reference date dataframe (DM-like)
  tgdt          = "DSSTDTC_DT", # Target date column (Date type)
  refdt         = "RFSTDTC",    # Reference date column (Date type)
  study_day_var = "DSSTDY",     # Output study day variable name
  merge_key     = "USUBJID"     # Key to join sdtm_in and dm_domain
)

# Step 4: Remove helper column
ds <- ds %>% select(-DSSTDTC_DT)

cat("  DSSTDY derived.\n\n")


# =============================================================================
# SECTION 9: Derive DSSEQ — Sequence Number
# =============================================================================
# derive_seq() correct API:
#   derive_seq(tgt_dat, tgt_var, rec_vars, sbj_vars, start_at)
# tgt_dat is the first argument — must be passed explicitly, not piped.

cat("STEP 7: Deriving DSSEQ using derive_seq()...\n")

ds <- derive_seq(
  tgt_dat  = ds,
  tgt_var  = "DSSEQ",
  rec_vars = c("USUBJID", "DSSTDTC")
)

cat("  DSSEQ derived.\n\n")


# =============================================================================
# SECTION 10: Select Final Variables in SDTM Order
# =============================================================================
# SDTM variable order: Identifiers -> Topic -> Qualifiers -> Timing

cat("STEP 8: Selecting final 12 variables in SDTM order...\n")

ds_final <- ds %>%
  select(
    STUDYID,   # Identifier: Study
    DOMAIN,    # Identifier: Domain = "DS"
    USUBJID,   # Identifier: Unique subject
    DSSEQ,     # Identifier: Sequence number
    DSTERM,    # Topic     : Verbatim disposition term
    DSDECOD,   # Qualifier : Standardized decoded term (from CT)
    DSCAT,     # Qualifier : Category of disposition event
    VISITNUM,  # Timing    : Visit number
    VISIT,     # Timing    : Visit name
    DSDTC,     # Timing    : Date of data collection (ISO 8601)
    DSSTDTC,   # Timing    : Start date of event (ISO 8601)
    DSSTDY     # Timing    : Study day of event
  )

cat(paste0("  Final: ", nrow(ds_final), " rows x ", ncol(ds_final), " cols\n"))
cat(paste0("  Vars : ", paste(names(ds_final), collapse = ", "), "\n\n"))


# =============================================================================
# SECTION 11: QC Checks
# =============================================================================

cat("STEP 9: Running QC Checks...\n")
cat("--------------------------------------------------------\n")

qc_pass <- TRUE

# QC 1: All required variables present
req_vars     <- c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM",
                  "DSDECOD","DSCAT","VISITNUM","VISIT",
                  "DSDTC","DSSTDTC","DSSTDY")
missing_vars <- setdiff(req_vars, names(ds_final))
if (length(missing_vars) == 0) {
  cat("  [PASS] QC  1: All 12 required variables present.\n")
} else {
  cat(paste0("  [FAIL] QC  1: Missing: ",
             paste(missing_vars, collapse = ", "), "\n"))
  qc_pass <- FALSE
}

# QC 2: DOMAIN = "DS"
if (all(ds_final$DOMAIN == "DS", na.rm = TRUE)) {
  cat("  [PASS] QC  2: DOMAIN = 'DS' for all records.\n")
} else {
  cat("  [FAIL] QC  2: Unexpected DOMAIN values.\n"); qc_pass <- FALSE
}

# QC 3: No missing STUDYID
n_miss <- sum(is.na(ds_final$STUDYID))
if (n_miss == 0) {
  cat("  [PASS] QC  3: No missing STUDYID.\n")
} else {
  cat(paste0("  [WARN] QC  3: ", n_miss, " missing STUDYID.\n"))
}

# QC 4: No missing USUBJID
n_miss <- sum(is.na(ds_final$USUBJID))
if (n_miss == 0) {
  cat("  [PASS] QC  4: No missing USUBJID.\n")
} else {
  cat(paste0("  [FAIL] QC  4: ", n_miss, " missing USUBJID.\n"))
  qc_pass <- FALSE
}

# QC 5: DSSEQ unique within USUBJID
dupes <- ds_final %>%
  group_by(USUBJID, DSSEQ) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if (nrow(dupes) == 0) {
  cat("  [PASS] QC  5: DSSEQ unique within each USUBJID.\n")
} else {
  cat(paste0("  [FAIL] QC  5: ", nrow(dupes), " duplicate DSSEQ.\n"))
  qc_pass <- FALSE
}

# QC 6: DSSEQ sequential from 1
seq_check <- ds_final %>%
  group_by(USUBJID) %>%
  summarise(min_s = min(DSSEQ), max_s = max(DSSEQ),
            n = n(), .groups = "drop") %>%
  filter(min_s != 1 | max_s != n)
if (nrow(seq_check) == 0) {
  cat("  [PASS] QC  6: DSSEQ sequential from 1 per subject.\n")
} else {
  cat(paste0("  [WARN] QC  6: ", nrow(seq_check),
             " subjects with non-sequential DSSEQ.\n"))
}

# QC 7: DSDECOD from CT where populated
invalid_ct <- ds_final %>%
  filter(!is.na(DSDECOD) & !(DSDECOD %in% study_ct$term_value))
if (nrow(invalid_ct) == 0) {
  cat("  [PASS] QC  7: All DSDECOD values from controlled terminology.\n")
} else {
  cat(paste0("  [WARN] QC  7: ", nrow(invalid_ct),
             " unrecognized DSDECOD: ",
             paste(unique(invalid_ct$DSDECOD), collapse = ", "), "\n"))
}

# QC 8: DSSTDTC valid ISO 8601
invalid_dtc <- ds_final %>%
  filter(!is.na(DSSTDTC)) %>%
  filter(is.na(as.Date(substr(DSSTDTC, 1, 10), format = "%Y-%m-%d")))
if (nrow(invalid_dtc) == 0) {
  cat("  [PASS] QC  8: All DSSTDTC are valid ISO 8601 dates.\n")
} else {
  cat(paste0("  [WARN] QC  8: ", nrow(invalid_dtc), " invalid DSSTDTC.\n"))
}

# QC 9: No DSSTDY = 0
zero_days <- ds_final %>% filter(!is.na(DSSTDY) & DSSTDY == 0)
if (nrow(zero_days) == 0) {
  cat("  [PASS] QC  9: No DSSTDY = 0 (day 0 invalid in SDTM).\n")
} else {
  cat(paste0("  [WARN] QC  9: ", nrow(zero_days), " DSSTDY = 0.\n"))
}

# QC 10: Primary key unique
pk_dupes <- ds_final %>%
  group_by(STUDYID, USUBJID, DSSEQ) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if (nrow(pk_dupes) == 0) {
  cat("  [PASS] QC 10: Primary key (STUDYID+USUBJID+DSSEQ) unique.\n")
} else {
  cat(paste0("  [FAIL] QC 10: ", nrow(pk_dupes), " duplicate PKs.\n"))
  qc_pass <- FALSE
}

cat(paste0("  [INFO] QC 11: ", nrow(ds_final), " records / ",
           n_distinct(ds_final$USUBJID), " subjects.\n"))

cat("--------------------------------------------------------\n")
cat(ifelse(qc_pass,
           "  OVERALL QC STATUS: ALL CRITICAL CHECKS PASSED\n",
           "  OVERALL QC STATUS: ONE OR MORE CRITICAL CHECKS FAILED\n"))
cat("\n")


# =============================================================================
# SECTION 12: Preview Output
# =============================================================================

cat("STEP 10: Dataset Preview...\n")
cat("--------------------------------------------------------\n")

cat("\n  DSDECOD frequency (CT mapping):\n")
print(table(ds_final$DSDECOD, useNA = "ifany"))

cat("\n  DSCAT frequency:\n")
print(table(ds_final$DSCAT, useNA = "ifany"))

cat("\n  VISIT frequency:\n")
print(table(ds_final$VISIT, useNA = "ifany"))

cat("\n  First 5 rows:\n")
print(head(ds_final, 5))

cat("\n  Structure:\n")
str(ds_final)


# =============================================================================
# SECTION 13: Save Outputs
# =============================================================================

cat("\nSTEP 11: Saving output files...\n")

saveRDS(ds_final, "ds_domain.rds")
cat("  Saved: ds_domain.rds\n")

write.csv(ds_final, "ds_domain.csv", row.names = FALSE, na = "")
cat("  Saved: ds_domain.csv\n")

cat("\n=============================================================\n")
cat("  DS Domain Creation COMPLETE\n")
cat(paste0("  Finished : ", Sys.time(), "\n"))
cat(paste0("  Records  : ", nrow(ds_final), "\n"))
cat(paste0("  Subjects : ", n_distinct(ds_final$USUBJID), "\n"))
cat("  Outputs  : ds_domain.rds, ds_domain.csv, question_1_log.txt\n")
cat("=============================================================\n")


# =============================================================================
# SECTION 14: Close Log
# =============================================================================
sink(type = "message")
sink(type = "output")
close(log_con)

message("Done. Check question_1_log.txt for full log.")
