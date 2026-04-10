# =============================================================================
# Script    : 01_create_ae_summary_table.R
# Purpose   : Create a regulatory-compliant summary table of Treatment-Emergent
#             Adverse Events (TEAEs) using {gtsummary}, similar to FDA Table 10.
#
# Assessment: Roche/Genentech ADS Programmer Coding Assessment - Question 3
#
# Author    : Ojaswi Bhimineni
# Date      : 2025
#
# Input Datasets:
#   pharmaverseadam::adae  : ADaM Adverse Events dataset
#   pharmaverseadam::adsl  : ADaM Subject Level dataset
#
# Output:
#   ae_summary_table.html  : Regulatory-style TEAE summary table
#   question_3a_log.txt    : Execution log confirming error-free run
#
# Table Structure (FDA Table 10 style):
#   Rows    : Primary System Organ Class (AESOC) with Adverse Event Term (AETERM)
#   Columns : Treatment arms (ACTARM) + Total
#   Values  : n (%) of subjects with each AE
#   Sorting : Descending frequency (most common AE SOC first)
#
# Reference:
#   FDA TLG Catalogue: https://pharmaverse.github.io/cardinal/quarto/index-catalog.html
#   gtsummary docs   : https://www.danieldsjoberg.com/gtsummary/
# =============================================================================


# =============================================================================
# SECTION 1: Load Required Packages
# =============================================================================

library(gtsummary)       # Clinical summary tables
library(pharmaverseadam) # ADaM example datasets
library(dplyr)           # Data manipulation
library(gt)              # Table formatting and HTML export
library(stringr)         # String operations


# =============================================================================
# SECTION 2: Configure Logging
# =============================================================================

log_file <- "question_3a_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, append = FALSE, type = "output")
sink(log_con, append = TRUE,  type = "message")

cat("=============================================================\n")
cat("  AE Summary Table Creation Log\n")
cat("  Roche/Genentech ADS Programmer Assessment - Question 3\n")
cat(paste0("  Run Timestamp: ", Sys.time(), "\n"))
cat("=============================================================\n\n")


# =============================================================================
# SECTION 3: Load and Prepare Input Datasets
# =============================================================================

cat("STEP 1: Loading input datasets...\n")

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

cat(paste0("  adae rows: ", nrow(adae), " | cols: ", ncol(adae), "\n"))
cat(paste0("  adsl rows: ", nrow(adsl), " | cols: ", ncol(adsl), "\n\n"))


# =============================================================================
# SECTION 4: Filter to Treatment-Emergent AEs (TEAEs)
# =============================================================================
# TEAEs are identified by TRTEMFL == "Y" in the ADAE dataset.
# Only subjects in ADSL (safety population) are included.
# Screen failures are excluded from the denominator.

cat("STEP 2: Filtering to TEAEs (TRTEMFL == 'Y')...\n")

# Get safety population from ADSL (exclude screen failures)
adsl_safe <- adsl %>%
  filter(!ACTARM %in% c("Screen Failure", ""))

# Filter ADAE to TEAEs only
adae_teae <- adae %>%
  filter(
    TRTEMFL == "Y",                          # Treatment-emergent only
    USUBJID %in% adsl_safe$USUBJID          # Safety population subjects only
  )

cat(paste0("  Safety population subjects : ", nrow(adsl_safe), "\n"))
cat(paste0("  TEAE records               : ", nrow(adae_teae), "\n"))
cat(paste0("  Unique subjects with TEAEs : ",
           n_distinct(adae_teae$USUBJID), "\n\n"))

# Treatment arm summary
cat("  Treatment arm distribution (safety population):\n")
print(table(adsl_safe$ACTARM))
cat("\n")


# =============================================================================
# SECTION 5: Prepare Data for gtsummary
# =============================================================================
# For AE summary tables, we need subject-level flags per AE term.
# The denominator is all subjects in the safety population (N per arm).
# Numerator is subjects with at least one TEAE for each term.
#
# Approach:
# 1. Create one record per subject per AESOC/AETERM combination (deduplicated)
# 2. Merge back to full safety population so subjects with no AEs appear
# 3. Use tbl_hierarchical() for SOC -> AETERM hierarchy with subject-level rates

cat("STEP 3: Preparing subject-level AE data for summary table...\n")

# Step 1: Deduplicate to one record per subject per AESOC/AETERM
# (a subject should only be counted once per term even if multiple events)
adae_subj <- adae_teae %>%
  select(STUDYID, USUBJID, AESOC, AETERM, ACTARM) %>%
  distinct(USUBJID, AESOC, AETERM, .keep_all = TRUE)

cat(paste0("  Subject-level AE records (deduplicated): ",
           nrow(adae_subj), "\n"))

# Step 2: Get denominator counts per treatment arm
denom <- adsl_safe %>%
  count(ACTARM, name = "N_ARM")

cat("  Denominator (N per arm):\n")
print(denom)
cat("\n")

# Step 3: Sort SOCs by descending frequency for ordering
soc_order <- adae_subj %>%
  group_by(AESOC) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(desc(n_subj)) %>%
  pull(AESOC)

# Apply factor ordering so gtsummary sorts by frequency
# IMPORTANT: ACTARM must be the same class (factor) in BOTH adae_subj
# and adsl_safe — gtsummary tbl_hierarchical() requires this to match

arm_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")

adae_subj <- adae_subj %>%
  mutate(
    AESOC  = factor(AESOC, levels = soc_order),
    ACTARM = factor(ACTARM, levels = arm_levels)
  )

# Make ACTARM a factor in adsl_safe with identical levels
# This is required so tbl_hierarchical() denominator class matches data class
adsl_safe <- adsl_safe %>%
  mutate(ACTARM = factor(ACTARM, levels = arm_levels))

cat("  SOC ordering (by descending frequency):\n")
print(soc_order)
cat("\n")


# =============================================================================
# SECTION 6: Build Summary Table Using {gtsummary}
# =============================================================================
# Use tbl_hierarchical() which is designed specifically for AE tables.
# It calculates subject-level rates with the correct denominator.
#
# Structure:
#   - variables: AESOC (System Organ Class) -> AETERM (preferred term)
#   - by: ACTARM (treatment arm columns)
#   - id: USUBJID (subject identifier for deduplication)
#   - denominator: adsl_safe (full safety population for % calculation)

cat("STEP 4: Building AE summary table using tbl_hierarchical()...\n")

ae_table <- tbl_hierarchical(
  data        = adae_subj,
  variables   = c(AESOC, AETERM),    # SOC -> Term hierarchy
  by          = ACTARM,              # Columns by treatment arm
  id          = USUBJID,             # Subject-level deduplication
  denominator = adsl_safe,           # Full safety population denominator
  overall_row = TRUE                 # Add overall "any TEAE" row at top
) %>%
  # Add total column across all treatment arms
  add_overall(last = TRUE) %>%
  # Bold the SOC (parent) labels for visual hierarchy
  bold_labels() %>%
  # Format column headers with N per arm
  modify_header(
    label ~ "**Primary System Organ Class**  \n**Reported Term for the Adverse Event**"
  ) %>%
  # Add table caption
  modify_caption(
    "**Table 1. Treatment-Emergent Adverse Events by System Organ Class and Preferred Term**  \n(Safety Population; Subjects with ≥1 TEAE shown)"
  ) %>%
  # Add footnote explaining the statistics
  modify_footnote_header(
    "n (%) = number and percentage of subjects with at least one event",
    columns = everything()
  )

cat("  Table built successfully.\n\n")


# =============================================================================
# SECTION 7: Save Output as HTML
# =============================================================================

cat("STEP 5: Saving AE summary table as HTML...\n")

ae_table %>%
  as_gt() %>%
  gt::tab_options(
    table.font.size    = 11,
    heading.align      = "left",
    column_labels.font.weight = "bold"
  ) %>%
  gt::gtsave(filename = "ae_summary_table.html")

cat("  Saved: ae_summary_table.html\n\n")


# =============================================================================
# SECTION 8: QC Summary
# =============================================================================

cat("STEP 6: QC Summary...\n")
cat("--------------------------------------------------------\n")
cat(paste0("  Total TEAE records    : ", nrow(adae_teae), "\n"))
cat(paste0("  Safety population N   : ", nrow(adsl_safe), "\n"))
cat(paste0("  Subjects with TEAEs   : ",
           n_distinct(adae_teae$USUBJID), "\n"))
cat(paste0("  Unique SOCs           : ",
           n_distinct(adae_teae$AESOC), "\n"))
cat(paste0("  Unique AE terms       : ",
           n_distinct(adae_teae$AETERM), "\n"))
cat("--------------------------------------------------------\n\n")

cat("=============================================================\n")
cat("  AE Summary Table Creation COMPLETE\n")
cat(paste0("  Finished  : ", Sys.time(), "\n"))
cat("  Output    : ae_summary_table.html\n")
cat("=============================================================\n")


# =============================================================================
# SECTION 9: Close Log
# =============================================================================
sink(type = "message")
sink(type = "output")
close(log_con)

message("Done. Check question_3a_log.txt for full log.")