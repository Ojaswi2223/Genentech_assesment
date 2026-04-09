# =============================================================================
# Script    : create_adsl.R
# Purpose   : Create ADaM Subject Level (ADSL) dataset using SDTM source
#             data and the {admiral} Pharmaverse package.
#
# Assessment: Roche/Genentech ADS Programmer Coding Assessment - Question 2
#
# Author    : Ojaswi Bhimineni
# Date      : 04/09/2026
#
# Input Datasets (all from pharmaversesdtm):
#   dm  : Demographics          - basis for ADSL
#   vs  : Vital Signs           - used for LSTAVLDT
#   ex  : Exposure              - used for TRTSDTM, TRTSTMF, TRTEDTM, LSTAVLDT
#   ds  : Disposition           - used for LSTAVLDT
#   ae  : Adverse Events        - used for LSTAVLDT
#
# Outputs:
#   adsl.rds          : Final ADSL dataset (R format)
#   adsl.csv          : Final ADSL dataset (CSV format)
#   question_2_log.txt: Execution log confirming error-free run
#
# Key Variables Derived:
#   AGEGR9 / AGEGR9N  : Age groupings (<18, 18-50, >50)
#   TRTSDTM / TRTSTMF : Treatment start datetime with imputation flag
#   TRTEDTM / TRTETMF : Treatment end datetime with imputation flag
#   TRTSDT  / TRTEDT  : Treatment start/end dates (from datetime)
#   ITTFL             : Intent-to-treat population flag
#   LSTAVLDT          : Last known alive date
#
# {admiral} Reference:
#   https://pharmaverse.github.io/admiral/cran-release/articles/adsl.html
#   https://pharmaverse.github.io/examples/adam/adsl
# =============================================================================


# =============================================================================
# SECTION 1: Load Required Packages
# =============================================================================

library(admiral)          # ADaM derivation functions
library(pharmaversesdtm)  # SDTM source datasets
library(dplyr)            # Data manipulation
library(lubridate)        # Date/time handling
library(stringr)          # String operations
library(tidyr)            # Data reshaping


# =============================================================================
# SECTION 2: Configure Logging
# =============================================================================
# Captures all output to question_2_log.txt as evidence of error-free run

log_file <- "question_2_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, append = FALSE, type = "output")
sink(log_con, append = TRUE,  type = "message")

cat("=============================================================\n")
cat("  ADaM ADSL Dataset Creation Log\n")
cat("  Roche/Genentech ADS Programmer Assessment - Question 2\n")
cat(paste0("  Run Timestamp: ", Sys.time(), "\n"))
cat("=============================================================\n\n")


# =============================================================================
# SECTION 3: Load and Prepare Source SDTM Datasets
# =============================================================================
# All input datasets are from pharmaversesdtm.
# convert_blanks_to_na() is an admiral utility that converts empty strings
# to NA — required before any admiral derivation functions.

cat("STEP 1: Loading SDTM source datasets...\n")

dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Convert blank strings to NA across all datasets
dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

cat(paste0("  dm rows : ", nrow(dm), " | cols: ", ncol(dm), "\n"))
cat(paste0("  vs rows : ", nrow(vs), " | cols: ", ncol(vs), "\n"))
cat(paste0("  ex rows : ", nrow(ex), " | cols: ", ncol(ex), "\n"))
cat(paste0("  ds rows : ", nrow(ds), " | cols: ", ncol(ds), "\n"))
cat(paste0("  ae rows : ", nrow(ae), " | cols: ", ncol(ae), "\n\n"))


# =============================================================================
# SECTION 4: Initialize ADSL from DM Domain
# =============================================================================
# Per ADaM ADSL specification, the DM domain is the basis for ADSL.
# We start by selecting the DM variables that carry over to ADSL directly,
# then derive additional analysis variables.
#
# Key DM variables retained:
#   STUDYID, USUBJID, SUBJID, SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
#   COUNTRY, ARM, ACTARM, RFSTDTC, RFENDTC, DTHDTC, DTHFL

cat("STEP 2: Initializing ADSL from DM domain...\n")

adsl <- dm %>%
  # Select all DM variables — ADSL is a superset of DM
  select(
    STUDYID, USUBJID, SUBJID, SITEID,
    AGE, AGEU, SEX, RACE, ETHNIC, COUNTRY,
    ARM, ACTARM,
    RFSTDTC, RFENDTC,
    DTHDTC, DTHFL
  ) %>%
  # Derive treatment group variables from ARM/ACTARM
  # TRT01P = planned treatment, TRT01A = actual treatment
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  )

cat(paste0("  ADSL initialized: ", nrow(adsl), " subjects\n\n"))


# =============================================================================
# SECTION 5: Derive Age Group Variables — AGEGR9 and AGEGR9N
# =============================================================================
# AGEGR9  : Character age group label
#   "<18"    : Age < 18
#   "18 - 50": Age >= 18 and <= 50
#   ">50"    : Age > 50
#
# AGEGR9N : Numeric code for AGEGR9
#   1 = "<18"
#   2 = "18 - 50"
#   3 = ">50"
#
# Source: DM.AGE

cat("STEP 3: Deriving AGEGR9 and AGEGR9N (age groupings)...\n")

adsl <- adsl %>%
  mutate(
    # Character age group label
    AGEGR9 = case_when(
      AGE < 18            ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50            ~ ">50",
      TRUE                ~ NA_character_
    ),
    # Numeric age group code (1, 2, 3)
    AGEGR9N = case_when(
      AGE < 18            ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50            ~ 3,
      TRUE                ~ NA_real_
    )
  )

cat("  AGEGR9 frequency:\n")
print(table(adsl$AGEGR9, useNA = "ifany"))
cat("  AGEGR9N frequency:\n")
print(table(adsl$AGEGR9N, useNA = "ifany"))
cat("\n")


# =============================================================================
# SECTION 6: Derive ITTFL — Intent-to-Treat Population Flag
# =============================================================================
# ITTFL = "Y" if the subject was randomized (DM.ARM is not missing)
# ITTFL = "N" if DM.ARM is missing (subject was not randomized)
#
# Source: DM.ARM

cat("STEP 4: Deriving ITTFL (Intent-to-Treat flag)...\n")

adsl <- adsl %>%
  mutate(
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
  )

cat("  ITTFL frequency:\n")
print(table(adsl$ITTFL, useNA = "ifany"))
cat("\n")


# =============================================================================
# SECTION 7: Derive Treatment Start/End Datetimes — TRTSDTM/TRTSTMF
# =============================================================================
# TRTSDTM: Treatment start datetime
#   = datetime of patient's FIRST exposure with a valid dose
#
# TRTSTMF: Treatment start time imputation flag
#   Populated when time component of EXSTDTC was imputed
#   NOT populated if only seconds were missing
#
# TRTEDTM: Treatment end datetime
#   = datetime of patient's LAST exposure with a valid dose
#
# Valid dose definition:
#   EXDOSE > 0  OR  (EXDOSE == 0 AND EXTRT contains "PLACEBO")
#
# Time imputation rules:
#   Completely missing time -> impute to 00:00:00
#   Partially missing hours -> impute to 00
#   Partially missing minutes -> impute to 00
#   Only seconds missing -> impute to 00 but DO NOT set TRTSTMF
#
# Source: EX domain (EXSTDTC, EXENDTC, EXDOSE, EXTRT, EXSEQ)

cat("STEP 5: Preparing EX dataset for treatment datetime derivation...\n")

# Pre-process EX: convert DTC strings to datetime with imputation
# derive_vars_dtm() converts ISO 8601 character to POSIXct datetime
# time_imputation = "first" imputes missing time to 00:00:00
ex_ext <- ex %>%
  # Derive EXSTDTM: exposure start datetime with time imputation
  derive_vars_dtm(
    dtc             = EXSTDTC,
    new_vars_prefix = "EXST",          # Creates EXSTDTM and EXSTTMF
    time_imputation = "first"          # Impute missing time to 00:00:00
  ) %>%
  # Derive EXENDTM: exposure end datetime with time imputation
  derive_vars_dtm(
    dtc             = EXENDTC,
    new_vars_prefix = "EXEN",          # Creates EXENDTM and EXENTMF
    time_imputation = "last"           # Impute missing time to 23:59:59
  )

cat("  EX pre-processing complete.\n")
cat(paste0("  EX rows: ", nrow(ex_ext), "\n\n"))

cat("STEP 6: Deriving TRTSDTM, TRTSTMF, TRTEDTM, TRTETMF...\n")

adsl <- adsl %>%
  # -------------------------------------------------------------------------
  # TRTSDTM / TRTSTMF: Treatment start datetime and imputation flag
  # Logic: First exposure record (by EXSTDTM, EXSEQ) with valid dose
  #        where date part of EXSTDTC is complete (EXSTDTM is not NA)
  # -------------------------------------------------------------------------
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
                     (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars    = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order       = exprs(EXSTDTM, EXSEQ),
    mode        = "first",              # Take the FIRST (earliest) record
    by_vars     = exprs(STUDYID, USUBJID)
  ) %>%

  # -------------------------------------------------------------------------
  # TRTEDTM / TRTETMF: Treatment end datetime and imputation flag
  # Logic: Last exposure record (by EXENDTM, EXSEQ) with valid dose
  #        where date part of EXENDTC is complete (EXENDTM is not NA)
  # -------------------------------------------------------------------------
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add  = (EXDOSE > 0 |
                     (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars    = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order       = exprs(EXENDTM, EXSEQ),
    mode        = "last",               # Take the LAST record
    by_vars     = exprs(STUDYID, USUBJID)
  )

cat("  TRTSDTM/TRTSTMF/TRTEDTM/TRTETMF derived.\n")
cat(paste0("  Subjects with TRTSDTM: ",
           sum(!is.na(adsl$TRTSDTM)), "\n"))
cat(paste0("  Subjects with TRTEDTM: ",
           sum(!is.na(adsl$TRTEDTM)), "\n\n"))


# =============================================================================
# SECTION 8: Derive Treatment Start/End Dates — TRTSDT and TRTEDT
# =============================================================================
# TRTSDT: Treatment start date (date part of TRTSDTM)
# TRTEDT: Treatment end date (date part of TRTEDTM)
# These are derived from the datetimes using derive_vars_dtm_to_dt()

cat("STEP 7: Deriving TRTSDT and TRTEDT from datetimes...\n")

adsl <- adsl %>%
  derive_vars_dtm_to_dt(
    source_vars = exprs(TRTSDTM, TRTEDTM)
  )

cat("  TRTSDT and TRTEDT derived.\n\n")


# =============================================================================
# SECTION 9: Derive LSTAVLDT — Last Known Alive Date
# =============================================================================
# LSTAVLDT = last date the subject had documented clinical evidence of
# being alive. Derived as the maximum of:
#
#   (1) Last complete VS visit date with a valid test result
#       - VS.VSDTC date part not missing
#       - VSSTRESN and VSSTRESC not BOTH missing (valid result)
#
#   (2) Last complete AE onset date
#       - AE.AESTDTC date part not missing
#
#   (3) Last complete DS disposition date
#       - DS.DSSTDTC date part not missing
#
#   (4) Last treatment date with valid dose
#       - Datepart of ADSL.TRTEDTM (already derived above)
#
# LSTAVLDT is stored as a Date variable.
# We use derive_var_extreme_dt() from admiral for each source.

cat("STEP 8: Deriving LSTAVLDT (Last Known Alive Date)...\n")

# ---- Source 1: Vital Signs — last visit date with valid test result ----
# Filter VS to records with:
#   - Valid test result (VSSTRESN or VSSTRESC is not missing)
#   - Complete visit date (VSDTC date part is not missing)

vs_ext <- vs %>%
  filter(
    # Valid result: at least one of numeric or character result is present
    (!is.na(VSSTRESN) | !is.na(VSSTRESC)) &
      # Complete date: VSDTC must be present and have a full date
      !is.na(VSDTC) & nchar(VSDTC) >= 10
  ) %>%
  # Convert VSDTC to Date
  derive_vars_dt(
    dtc             = VSDTC,
    new_vars_prefix = "VS"     # Creates VSDT
  )

cat(paste0("  VS records with valid result and complete date: ",
           nrow(vs_ext), "\n"))


# ---- Source 2: Adverse Events — last complete AE onset date ----
ae_ext <- ae %>%
  filter(
    # Complete AE start date: AESTDTC must have at least a full date
    !is.na(AESTDTC) & nchar(AESTDTC) >= 10
  ) %>%
  derive_vars_dt(
    dtc             = AESTDTC,
    new_vars_prefix = "AEST"   # Creates AESTDT
  )

cat(paste0("  AE records with complete onset date: ",
           nrow(ae_ext), "\n"))


# ---- Source 3: Disposition — last complete disposition date ----
ds_ext <- ds %>%
  filter(
    # Complete disposition start date
    !is.na(DSSTDTC) & nchar(DSSTDTC) >= 10
  ) %>%
  derive_vars_dt(
    dtc             = DSSTDTC,
    new_vars_prefix = "DSST"   # Creates DSSTDT
  )

cat(paste0("  DS records with complete disposition date: ",
           nrow(ds_ext), "\n"))


# ---- Derive LSTAVLDT as max of all four sources ----
# Use derive_vars_extreme_event() which finds the most extreme (max) date
# across multiple source datasets per subject.

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # Source 1: Vital signs visit dates with valid result
      event(
        dataset_name  = "vs_ext",
        condition     = !is.na(VSDT),
        set_values_to = exprs(LSTAVLDT = VSDT)
      ),
      # Source 2: Last complete AE onset date
      event(
        dataset_name  = "ae_ext",
        condition     = !is.na(AESTDT),
        set_values_to = exprs(LSTAVLDT = AESTDT)
      ),
      # Source 3: Last complete disposition date
      event(
        dataset_name  = "ds_ext",
        condition     = !is.na(DSSTDT),
        set_values_to = exprs(LSTAVLDT = DSSTDT)
      ),
      # Source 4: Last treatment date with valid dose
      event(
        dataset_name  = "adsl",
        condition     = !is.na(TRTEDT),
        set_values_to = exprs(LSTAVLDT = TRTEDT)
      )
    ),
    source_datasets = list(
      vs_ext = vs_ext,
      ae_ext = ae_ext,
      ds_ext = ds_ext,
      adsl   = adsl
    ),
    new_vars = exprs(LSTAVLDT),
    order    = exprs(LSTAVLDT),  # Required: order by the date variable
    mode     = "last"            # Take the LATEST (most recent) date
  )

cat(paste0("  Subjects with LSTAVLDT: ",
           sum(!is.na(adsl$LSTAVLDT)), "\n\n"))


# =============================================================================
# SECTION 10: Derive Reference Start/End Dates
# =============================================================================
# RFSTDT: Reference Start Date (date part of RFSTDTC from DM)
# RFENDT: Reference End Date (date part of RFENDTC from DM)
# These are standard ADSL variables used as baseline references.

cat("STEP 9: Deriving RFSTDT and RFENDT from DM reference dates...\n")

adsl <- adsl %>%
  mutate(
    RFSTDT = as.Date(substr(RFSTDTC, 1, 10), format = "%Y-%m-%d"),
    RFENDT = as.Date(substr(RFENDTC, 1, 10), format = "%Y-%m-%d")
  )

cat("  RFSTDT and RFENDT derived.\n\n")


# =============================================================================
# SECTION 11: Final Variable Selection and Ordering
# =============================================================================
# Select and order variables per ADaM ADSL specification.
# Identifier variables first, then subject-level characteristics,
# then treatment variables, then analysis flags/dates.

cat("STEP 10: Selecting and ordering final ADSL variables...\n")

adsl_final <- adsl %>%
  select(
    # --- Identifier Variables ---
    STUDYID,    # Study identifier
    USUBJID,    # Unique subject identifier
    SUBJID,     # Subject identifier within site
    SITEID,     # Study site identifier

    # --- Subject Characteristics ---
    AGE,        # Age at study start
    AGEU,       # Age units
    AGEGR9,     # Age group label (NEW: <18, 18-50, >50)
    AGEGR9N,    # Age group numeric (NEW: 1, 2, 3)
    SEX,        # Sex
    RACE,       # Race
    ETHNIC,     # Ethnicity
    COUNTRY,    # Country

    # --- Treatment Variables ---
    ARM,        # Planned treatment arm
    ACTARM,     # Actual treatment arm
    TRT01P,     # Planned treatment period 1
    TRT01A,     # Actual treatment period 1

    # --- Treatment Dates/Times ---
    TRTSDTM,    # Treatment start datetime (NEW)
    TRTSTMF,    # Treatment start time imputation flag (NEW)
    TRTSDT,     # Treatment start date
    TRTEDTM,    # Treatment end datetime
    TRTETMF,    # Treatment end time imputation flag
    TRTEDT,     # Treatment end date

    # --- Reference Dates ---
    RFSTDTC,    # Reference start date/time (DM)
    RFENDTC,    # Reference end date/time (DM)
    RFSTDT,     # Reference start date
    RFENDT,     # Reference end date

    # --- Death Variables ---
    DTHDTC,     # Date of death
    DTHFL,      # Death flag

    # --- Population Flags ---
    ITTFL,      # Intent-to-treat flag (NEW)

    # --- Survival/Last Contact ---
    LSTAVLDT    # Last known alive date (NEW)
  )

cat(paste0("  Final ADSL: ", nrow(adsl_final), " rows x ",
           ncol(adsl_final), " columns\n"))
cat(paste0("  Variables : ", paste(names(adsl_final), collapse = ", "),
           "\n\n"))


# =============================================================================
# SECTION 12: Data Quality Checks
# =============================================================================

cat("STEP 11: Running QC Checks...\n")
cat("--------------------------------------------------------\n")

qc_pass <- TRUE

# QC 1: One record per subject (ADSL is subject-level)
dupes <- adsl_final %>%
  group_by(USUBJID) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)
if (nrow(dupes) == 0) {
  cat("  [PASS] QC  1: One record per USUBJID (subject-level).\n")
} else {
  cat(paste0("  [FAIL] QC  1: ", nrow(dupes),
             " subjects with duplicate records.\n"))
  qc_pass <- FALSE
}

# QC 2: No missing STUDYID
if (sum(is.na(adsl_final$STUDYID)) == 0) {
  cat("  [PASS] QC  2: No missing STUDYID.\n")
} else {
  cat(paste0("  [FAIL] QC  2: ",
             sum(is.na(adsl_final$STUDYID)), " missing STUDYID.\n"))
  qc_pass <- FALSE
}

# QC 3: No missing USUBJID
if (sum(is.na(adsl_final$USUBJID)) == 0) {
  cat("  [PASS] QC  3: No missing USUBJID.\n")
} else {
  cat(paste0("  [FAIL] QC  3: ",
             sum(is.na(adsl_final$USUBJID)), " missing USUBJID.\n"))
  qc_pass <- FALSE
}

# QC 4: AGEGR9 values are valid
valid_agegr9 <- c("<18", "18 - 50", ">50")
invalid_age  <- adsl_final %>%
  filter(!is.na(AGEGR9) & !(AGEGR9 %in% valid_agegr9))
if (nrow(invalid_age) == 0) {
  cat("  [PASS] QC  4: All AGEGR9 values are valid.\n")
} else {
  cat(paste0("  [WARN] QC  4: ", nrow(invalid_age),
             " unexpected AGEGR9 values.\n"))
}

# QC 5: AGEGR9N is 1, 2, or 3
invalid_agen <- adsl_final %>%
  filter(!is.na(AGEGR9N) & !(AGEGR9N %in% c(1, 2, 3)))
if (nrow(invalid_agen) == 0) {
  cat("  [PASS] QC  5: All AGEGR9N values are 1, 2, or 3.\n")
} else {
  cat(paste0("  [WARN] QC  5: ", nrow(invalid_agen),
             " unexpected AGEGR9N values.\n"))
}

# QC 6: AGEGR9 and AGEGR9N are consistent
age_mismatch <- adsl_final %>%
  filter(
    (!is.na(AGEGR9) & !is.na(AGEGR9N)) &
      !((AGEGR9 == "<18"     & AGEGR9N == 1) |
          (AGEGR9 == "18 - 50" & AGEGR9N == 2) |
          (AGEGR9 == ">50"     & AGEGR9N == 3))
  )
if (nrow(age_mismatch) == 0) {
  cat("  [PASS] QC  6: AGEGR9 and AGEGR9N are consistent.\n")
} else {
  cat(paste0("  [FAIL] QC  6: ", nrow(age_mismatch),
             " AGEGR9/AGEGR9N mismatches.\n"))
  qc_pass <- FALSE
}

# QC 7: ITTFL only contains "Y" or "N"
invalid_itt <- adsl_final %>%
  filter(!is.na(ITTFL) & !(ITTFL %in% c("Y", "N")))
if (nrow(invalid_itt) == 0) {
  cat("  [PASS] QC  7: ITTFL only contains 'Y' or 'N'.\n")
} else {
  cat(paste0("  [FAIL] QC  7: ", nrow(invalid_itt),
             " invalid ITTFL values.\n"))
  qc_pass <- FALSE
}

# QC 8: No missing ITTFL
if (sum(is.na(adsl_final$ITTFL)) == 0) {
  cat("  [PASS] QC  8: No missing ITTFL values.\n")
} else {
  cat(paste0("  [FAIL] QC  8: ",
             sum(is.na(adsl_final$ITTFL)), " missing ITTFL.\n"))
  qc_pass <- FALSE
}

# QC 9: TRTSDTM <= TRTEDTM where both exist
trt_order <- adsl_final %>%
  filter(!is.na(TRTSDTM) & !is.na(TRTEDTM)) %>%
  filter(TRTSDTM > TRTEDTM)
if (nrow(trt_order) == 0) {
  cat("  [PASS] QC  9: TRTSDTM <= TRTEDTM for all subjects.\n")
} else {
  cat(paste0("  [WARN] QC  9: ", nrow(trt_order),
             " subjects with TRTSDTM > TRTEDTM.\n"))
}

# QC 10: LSTAVLDT >= TRTSDT where both exist
last_alive <- adsl_final %>%
  filter(!is.na(LSTAVLDT) & !is.na(TRTSDT)) %>%
  filter(LSTAVLDT < TRTSDT)
if (nrow(last_alive) == 0) {
  cat("  [PASS] QC 10: LSTAVLDT >= TRTSDT for all subjects.\n")
} else {
  cat(paste0("  [WARN] QC 10: ", nrow(last_alive),
             " subjects with LSTAVLDT < TRTSDT.\n"))
}

# QC 11: Summary counts
cat(paste0("  [INFO] QC 11: ", nrow(adsl_final), " total subjects.\n"))
cat(paste0("  [INFO] QC 12: ", sum(adsl_final$ITTFL == "Y"),
           " subjects in ITT population.\n"))
cat(paste0("  [INFO] QC 13: ",
           sum(!is.na(adsl_final$TRTSDTM)), " subjects treated.\n"))

cat("--------------------------------------------------------\n")
cat(ifelse(qc_pass,
           "  OVERALL QC STATUS: ALL CRITICAL CHECKS PASSED\n",
           "  OVERALL QC STATUS: ONE OR MORE CRITICAL CHECKS FAILED\n"))
cat("\n")


# =============================================================================
# SECTION 13: Preview Final Dataset
# =============================================================================

cat("STEP 12: Dataset Preview...\n")
cat("--------------------------------------------------------\n")

cat("\n  AGEGR9 frequency:\n")
print(table(adsl_final$AGEGR9, useNA = "ifany"))

cat("\n  AGEGR9N frequency:\n")
print(table(adsl_final$AGEGR9N, useNA = "ifany"))

cat("\n  ITTFL frequency:\n")
print(table(adsl_final$ITTFL, useNA = "ifany"))

cat("\n  TRTSTMF frequency (time imputation flag):\n")
print(table(adsl_final$TRTSTMF, useNA = "ifany"))

cat("\n  LSTAVLDT summary:\n")
print(summary(adsl_final$LSTAVLDT))

cat("\n  First 5 rows (key variables):\n")
print(
  adsl_final %>%
    select(USUBJID, AGE, AGEGR9, AGEGR9N, ITTFL,
           TRTSDTM, TRTSTMF, LSTAVLDT) %>%
    head(5)
)

cat("\n  Full dataset structure:\n")
str(adsl_final)


# =============================================================================
# SECTION 14: Save Output Files
# =============================================================================

cat("\nSTEP 13: Saving output files...\n")

saveRDS(adsl_final, "adsl.rds")
cat("  Saved: adsl.rds\n")

write.csv(adsl_final, "adsl.csv", row.names = FALSE, na = "")
cat("  Saved: adsl.csv\n")

cat("\n=============================================================\n")
cat("  ADSL Creation COMPLETE\n")
cat(paste0("  Finished   : ", Sys.time(), "\n"))
cat(paste0("  Subjects   : ", nrow(adsl_final), "\n"))
cat(paste0("  Variables  : ", ncol(adsl_final), "\n"))
cat(paste0("  ITT subjects: ", sum(adsl_final$ITTFL == "Y"), "\n"))
cat("  Outputs    : adsl.rds, adsl.csv, question_2_log.txt\n")
cat("=============================================================\n")


# =============================================================================
# SECTION 15: Close Log
# =============================================================================
sink(type = "message")
sink(type = "output")
close(log_con)

message("Done. Check question_2_log.txt for full log.")
