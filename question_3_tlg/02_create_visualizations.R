# =============================================================================
# Script    : 02_create_visualizations.R
# Purpose   : Create two ggplot2 visualizations for adverse events reporting:
#             Plot 1 - AE severity distribution by treatment arm (bar chart)
#             Plot 2 - Top 10 most frequent AEs with 95% CI (forest plot)
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
# Outputs:
#   plot1_ae_severity.png  : AE severity distribution by treatment (bar chart)
#   plot2_top10_ae.png     : Top 10 AEs with 95% CI forest plot
#   question_3b_log.txt    : Execution log confirming error-free run
#
# Reference:
#   ggplot2 docs: https://ggplot2.tidyverse.org/
#   FDA TLG Catalogue: https://pharmaverse.github.io/cardinal/
# =============================================================================


# =============================================================================
# SECTION 1: Load Required Packages
# =============================================================================

library(ggplot2)         # Visualization
library(pharmaverseadam) # ADaM example datasets
library(dplyr)           # Data manipulation
library(tidyr)           # Data reshaping
library(stringr)         # String wrapping for labels
library(scales)          # Axis formatting (percent_format)


# =============================================================================
# SECTION 2: Configure Logging
# =============================================================================

log_file <- "question_3b_log.txt"
log_con  <- file(log_file, open = "wt")
sink(log_con, append = FALSE, type = "output")
sink(log_con, append = TRUE,  type = "message")

cat("=============================================================\n")
cat("  AE Visualizations Log\n")
cat("  Roche/Genentech ADS Programmer Assessment - Question 3\n")
cat(paste0("  Run Timestamp: ", Sys.time(), "\n"))
cat("=============================================================\n\n")


# =============================================================================
# SECTION 3: Load and Prepare Data
# =============================================================================

cat("STEP 1: Loading input datasets...\n")

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Safety population: exclude screen failures
adsl_safe <- adsl %>%
  filter(!ACTARM %in% c("Screen Failure", ""))

# Filter to TEAEs in safety population
adae_teae <- adae %>%
  filter(
    TRTEMFL == "Y",
    USUBJID %in% adsl_safe$USUBJID
  )

# N per treatment arm (for percentage calculations)
n_by_arm <- adsl_safe %>%
  count(ACTARM, name = "N_ARM")

# Define consistent treatment arm factor levels and colors
arm_levels <- c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose")
arm_colors <- c(
  "Placebo"               = "#4E79A7",
  "Xanomeline Low Dose"   = "#F28E2B",
  "Xanomeline High Dose"  = "#E15759"
)

cat(paste0("  TEAE records: ", nrow(adae_teae), "\n"))
cat(paste0("  Safety population: ", nrow(adsl_safe), " subjects\n\n"))


# =============================================================================
# SECTION 4: PLOT 1 — AE Severity Distribution by Treatment
# =============================================================================
# Bar chart showing count of AEs by severity level (AESEV) per treatment arm.
# AESEV values: MILD, MODERATE, SEVERE
# One stacked bar per treatment arm, colored by severity level.

cat("STEP 2: Creating Plot 1 — AE Severity Distribution by Treatment...\n")

# Prepare severity data
# Count AEs by treatment arm and severity level
severity_data <- adae_teae %>%
  filter(!is.na(AESEV)) %>%
  mutate(
    ACTARM = factor(ACTARM, levels = arm_levels),
    # Order severity levels from least to most severe
    AESEV  = factor(AESEV,
                    levels = c("MILD", "MODERATE", "SEVERE"),
                    labels = c("Mild", "Moderate", "Severe"))
  ) %>%
  group_by(ACTARM, AESEV) %>%
  summarise(n_ae = n(), .groups = "drop")

cat("  Severity data summary:\n")
print(severity_data)
cat("\n")

# Severity color palette (clinical standard: green/yellow/red)
severity_colors <- c(
  "Mild"     = "#59A14F",   # Green
  "Moderate" = "#F28E2B",   # Orange
  "Severe"   = "#E15759"    # Red
)

# Build the stacked bar chart
plot1 <- ggplot(
  severity_data,
  aes(x = ACTARM, y = n_ae, fill = AESEV)
) +
  geom_bar(
    stat     = "identity",
    position = "stack",
    width    = 0.6,
    color    = "white",     # White borders between stacks
    linewidth = 0.3
  ) +
  # Add count labels inside each bar segment
  geom_text(
    aes(label = n_ae),
    position = position_stack(vjust = 0.5),
    color    = "white",
    size     = 3.5,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = severity_colors,
    name   = "Severity/Intensity"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "AE severity distribution by treatment",
    subtitle = "Treatment-Emergent Adverse Events (TEAEs); Safety Population",
    x        = "Treatment Arm",
    y        = "Count of AEs",
    caption  = paste0(
      "Source: pharmaverseadam::adae | TRTEMFL = 'Y'\n",
      "N (Safety Pop): ",
      paste(
        paste0(arm_levels, " n=",
               n_by_arm$N_ARM[match(arm_levels, n_by_arm$ACTARM)]),
        collapse = "; "
      )
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "gray40"),
    plot.caption     = element_text(size = 8, color = "gray50",
                                    hjust = 0),
    axis.title       = element_text(face = "bold"),
    axis.text.x      = element_text(size = 10),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Save Plot 1
ggsave(
  filename = "plot1_ae_severity.png",
  plot     = plot1,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

cat("  Saved: plot1_ae_severity.png\n\n")


# =============================================================================
# SECTION 5: PLOT 2 — Top 10 Most Frequent AEs with 95% CI
# =============================================================================
# Forest/dot plot showing the top 10 most frequent adverse event terms (AETERM)
# with 95% Clopper-Pearson confidence intervals for incidence rates.
#
# Method:
#   - Denominator = all subjects in safety population
#   - Numerator   = subjects with at least one occurrence of each AETERM
#   - 95% CI      = Clopper-Pearson exact binomial CI
#   - Sorted by descending incidence rate

cat("STEP 3: Creating Plot 2 — Top 10 AEs with 95% CI...\n")

# Total safety population N
N_total <- nrow(adsl_safe)

# Deduplicate: one record per subject per AETERM
ae_subj <- adae_teae %>%
  distinct(USUBJID, AETERM)

# Count subjects per AETERM and calculate incidence + 95% Clopper-Pearson CI
ae_counts <- ae_subj %>%
  group_by(AETERM) %>%
  summarise(n_subj = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    N     = N_total,
    # Incidence rate as proportion
    rate  = n_subj / N,
    # 95% Clopper-Pearson exact binomial confidence interval
    # qbeta() is the exact method (same as binom.test)
    ci_lo = qbeta(0.025, n_subj,     N - n_subj + 1),
    ci_hi = qbeta(0.975, n_subj + 1, N - n_subj)
  ) %>%
  arrange(desc(rate))

# Select top 10
top10_ae <- ae_counts %>%
  slice_head(n = 10) %>%
  # Order factor by ascending rate for horizontal plot (bottom = highest)
  mutate(
    AETERM = factor(AETERM, levels = rev(AETERM))
  )

cat("  Top 10 AEs by incidence rate:\n")
print(
  top10_ae %>%
    select(AETERM, n_subj, rate, ci_lo, ci_hi) %>%
    mutate(
      rate  = round(rate * 100, 1),
      ci_lo = round(ci_lo * 100, 1),
      ci_hi = round(ci_hi * 100, 1)
    )
)
cat("\n")

# Build the forest plot
plot2 <- ggplot(
  top10_ae,
  aes(x = rate, y = AETERM)
) +
  # Confidence interval lines
  geom_errorbarh(
    aes(xmin = ci_lo, xmax = ci_hi),
    height    = 0.25,
    color     = "gray40",
    linewidth = 0.6
  ) +
  # Point estimate dot
  geom_point(
    size  = 3,
    color = "#4E79A7",
    shape = 16
  ) +
  # Reference line at 10%
  geom_vline(
    xintercept = 0.10,
    linetype   = "dashed",
    color      = "gray60",
    linewidth  = 0.4
  ) +
  # X axis as percentage
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(top10_ae$ci_hi) * 1.1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title    = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0(
      "n = ", N_total,
      " subjects; 95% Clopper-Pearson CIs"
    ),
    x        = "Percentage of Patients (%)",
    y        = NULL,
    caption  = paste0(
      "Source: pharmaverseadam::adae | TRTEMFL = 'Y'\n",
      "Subject-level rates: each subject counted once per AETERM"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title     = element_text(face = "bold", size = 13),
    plot.subtitle  = element_text(size = 10, color = "gray40"),
    plot.caption   = element_text(size = 8, color = "gray50", hjust = 0),
    axis.title.x   = element_text(face = "bold"),
    axis.text.y    = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Save Plot 2
ggsave(
  filename = "plot2_top10_ae.png",
  plot     = plot2,
  width    = 8,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

cat("  Saved: plot2_top10_ae.png\n\n")


# =============================================================================
# SECTION 6: QC Summary
# =============================================================================

cat("STEP 4: QC Summary...\n")
cat("--------------------------------------------------------\n")
cat(paste0("  Total TEAE records  : ", nrow(adae_teae), "\n"))
cat(paste0("  Safety population N : ", N_total, "\n"))
cat(paste0("  Unique AE terms     : ",
           n_distinct(adae_teae$AETERM), "\n"))
cat(paste0("  Severity levels     : ",
           paste(unique(adae_teae$AESEV), collapse = ", "), "\n"))
cat(paste0("  Plot 1 saved        : plot1_ae_severity.png\n"))
cat(paste0("  Plot 2 saved        : plot2_top10_ae.png\n"))
cat("--------------------------------------------------------\n\n")

cat("=============================================================\n")
cat("  AE Visualizations COMPLETE\n")
cat(paste0("  Finished : ", Sys.time(), "\n"))
cat("  Outputs  : plot1_ae_severity.png, plot2_top10_ae.png\n")
cat("=============================================================\n")


# =============================================================================
# SECTION 7: Close Log
# =============================================================================
sink(type = "message")
sink(type = "output")
close(log_con)

message("Done. Check question_3b_log.txt for full log.")
