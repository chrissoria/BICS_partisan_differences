# =============================================================================
# Weight Sensitivity Analysis (Descriptive and Regression)
# =============================================================================
# This script compares results across three weighting scenarios:
#   1. Unweighted (no weights)
#   2. Original weights (weight_pooled) - demographic adjustment only
#   3. Raked weights (weight_party_raked) - demographic + party adjustment
#
# Includes:
#   - Descriptive statistics (weighted means by party)
#   - Partisan gaps in health behaviors
#   - Regression-based comparisons
#   - Wave-level analysis
#
# Output files:
#   - out/tables/weight_sensitivity_summary.csv (descriptive)
#   - out/tables/regression_weight_sensitivity.csv (regression)
# =============================================================================

library(tidyverse)
library(emmeans)
library(broom)
library(here)
library(knitr)
library(kableExtra)

# =============================================================================
# 1. SETUP AND DATA LOADING
# =============================================================================

# First, run the party weights script to create weight_party_raked
source(here("code", "00a-Create_Party_Weights.R"))

# Filter to valid parties
valid_parties <- c("Democrat", "Republican", "Independent")

bics_valid <- bics_zip_features %>%
  filter(political_party %in% valid_parties) %>%
  mutate(political_party = factor(political_party,
                                   levels = c("Republican", "Independent", "Democrat")))

# Create party-specific dataframes for regressions
RD_df <- bics_valid %>% filter(political_party %in% c("Republican", "Democrat"))
ID_df <- bics_valid %>% filter(political_party %in% c("Independent", "Democrat"))

cat("\n\n")
cat("=============================================================================\n")
cat("WEIGHT SENSITIVITY ANALYSIS\n")
cat("=============================================================================\n")

# =============================================================================
# PART I: DESCRIPTIVE ANALYSIS
# =============================================================================

# =============================================================================
# 2. HELPER FUNCTIONS FOR WEIGHTED MEANS
# =============================================================================

calc_weighted_stats <- function(data, weight_var, group_var = "political_party") {

  # Handle unweighted case
  if (is.null(weight_var) || weight_var == "none") {
    data$w <- 1
  } else {
    data$w <- data[[weight_var]]
  }

  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      n = n(),
      total_contacts = weighted.mean(num_cc, w = w, na.rm = TRUE),
      non_hh_contacts = weighted.mean(num_cc_nonhh, w = w, na.rm = TRUE),
      mask_usage = weighted.mean(Norm_Masks_Used, w = w, na.rm = TRUE) * 100,
      vaccinated = weighted.mean(Vaccinated, w = w, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

calc_weighted_demographics <- function(data, weight_var, group_var = "political_party") {

  if (is.null(weight_var) || weight_var == "none") {
    data$w <- 1
  } else {
    data$w <- data[[weight_var]]
  }

  data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      n = n(),
      avg_age = weighted.mean(age, w = w, na.rm = TRUE),
      pct_male = weighted.mean(male, w = w, na.rm = TRUE) * 100,
      pct_white = weighted.mean(white, w = w, na.rm = TRUE) * 100,
      pct_metro = weighted.mean(Metro, w = w, na.rm = TRUE) * 100,
      pct_college = (weighted.mean(as.numeric(r_college_grad), w = w, na.rm = TRUE) - 1) * 100,
      pct_employed = weighted.mean(r_working_num, w = w, na.rm = TRUE) * 100,
      .groups = "drop"
    )
}

# =============================================================================
# 3. COMPARE HEALTH BEHAVIORS ACROSS WEIGHTING SCENARIOS
# =============================================================================

cat("\n=============================================================================\n")
cat("TABLE 1: HEALTH BEHAVIORS BY PARTY - SENSITIVITY ANALYSIS\n")
cat("=============================================================================\n")

# Calculate for each weighting scenario
behaviors_unweighted <- calc_weighted_stats(bics_valid, "none") %>%
  mutate(weight_type = "Unweighted")

behaviors_pooled <- calc_weighted_stats(bics_valid, "weight_pooled") %>%
  mutate(weight_type = "Original (weight_pooled)")

behaviors_raked <- calc_weighted_stats(bics_valid, "weight_party_raked") %>%
  mutate(weight_type = "Raked (weight_party_raked)")

# Combine
behaviors_comparison <- bind_rows(
  behaviors_unweighted,
  behaviors_pooled,
  behaviors_raked
) %>%
  select(weight_type, political_party, everything())

# Print by behavior
cat("\n--- Total Daily Contacts ---\n")
behaviors_comparison %>%
  select(weight_type, political_party, total_contacts) %>%
  pivot_wider(names_from = political_party, values_from = total_contacts) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()

cat("\n--- Non-Household Contacts ---\n")
behaviors_comparison %>%
  select(weight_type, political_party, non_hh_contacts) %>%
  pivot_wider(names_from = political_party, values_from = non_hh_contacts) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()

cat("\n--- % Contacts with Mask ---\n")
behaviors_comparison %>%
  select(weight_type, political_party, mask_usage) %>%
  pivot_wider(names_from = political_party, values_from = mask_usage) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print()

cat("\n--- % Vaccinated (Wave 6 only) ---\n")
# Vaccination is only measured in wave 6
bics_wave6 <- bics_valid %>% filter(wave == 6)

vax_unweighted <- calc_weighted_stats(bics_wave6, "none") %>%
  mutate(weight_type = "Unweighted") %>%
  select(weight_type, political_party, vaccinated)

vax_pooled <- calc_weighted_stats(bics_wave6, "weight_pooled") %>%
  mutate(weight_type = "Original (weight_pooled)") %>%
  select(weight_type, political_party, vaccinated)

vax_raked <- calc_weighted_stats(bics_wave6, "weight_party_raked") %>%
  mutate(weight_type = "Raked (weight_party_raked)") %>%
  select(weight_type, political_party, vaccinated)

bind_rows(vax_unweighted, vax_pooled, vax_raked) %>%
  pivot_wider(names_from = political_party, values_from = vaccinated) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print()

# =============================================================================
# 4. COMPARE PARTISAN GAPS ACROSS WEIGHTING SCENARIOS
# =============================================================================

cat("\n=============================================================================\n")
cat("PARTISAN GAPS (REPUBLICAN - DEMOCRAT)\n")
cat("=============================================================================\n")

calc_partisan_gaps <- function(data, weight_var) {
  stats <- calc_weighted_stats(data, weight_var)

  rep_stats <- stats %>% filter(political_party == "Republican")
  dem_stats <- stats %>% filter(political_party == "Democrat")

  tibble(
    contacts_gap = rep_stats$total_contacts - dem_stats$total_contacts,
    non_hh_gap = rep_stats$non_hh_contacts - dem_stats$non_hh_contacts,
    mask_gap = rep_stats$mask_usage - dem_stats$mask_usage
  )
}

gaps_comparison <- tibble(
  weight_type = c("Unweighted", "Original (weight_pooled)", "Raked (weight_party_raked)")
) %>%
  mutate(
    gaps = list(
      calc_partisan_gaps(bics_valid, "none"),
      calc_partisan_gaps(bics_valid, "weight_pooled"),
      calc_partisan_gaps(bics_valid, "weight_party_raked")
    )
  ) %>%
  unnest(gaps)

cat("\nRepublican - Democrat differences:\n")
gaps_comparison %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  rename(
    `Contacts Gap` = contacts_gap,
    `Non-HH Contacts Gap` = non_hh_gap,
    `Mask Usage Gap (pp)` = mask_gap
  ) %>%
  print()

# Vaccination gap (wave 6 only)
calc_vax_gap <- function(data, weight_var) {
  stats <- calc_weighted_stats(data, weight_var)
  rep_vax <- stats %>% filter(political_party == "Republican") %>% pull(vaccinated)
  dem_vax <- stats %>% filter(political_party == "Democrat") %>% pull(vaccinated)
  rep_vax - dem_vax
}

cat("\nVaccination Gap (Republican - Democrat, Wave 6):\n")
cat(sprintf("  Unweighted: %.1f pp\n", calc_vax_gap(bics_wave6, "none")))
cat(sprintf("  Original:   %.1f pp\n", calc_vax_gap(bics_wave6, "weight_pooled")))
cat(sprintf("  Raked:      %.1f pp\n", calc_vax_gap(bics_wave6, "weight_party_raked")))

# =============================================================================
# 5. SAMPLE PROPORTIONS CHECK
# =============================================================================

cat("\n=============================================================================\n")
cat("SAMPLE PROPORTIONS BY WEIGHTING SCENARIO\n")
cat("=============================================================================\n")

sample_props_check <- bics_valid %>%
  group_by(political_party) %>%
  summarise(
    n = n(),
    unweighted_prop = n / nrow(bics_valid),
    pooled_prop = sum(weight_pooled) / sum(bics_valid$weight_pooled),
    raked_prop = sum(weight_party_raked) / sum(bics_valid$weight_party_raked),
    .groups = "drop"
  ) %>%
  mutate(
    gallup_target = c(0.296, 0.398, 0.306)[match(political_party, c("Republican", "Independent", "Democrat"))]
  )

cat("\n")
sample_props_check %>%
  mutate(across(where(is.numeric) & !matches("^n$"), ~round(., 3))) %>%
  print()

# =============================================================================
# 6. DESCRIPTIVE SUMMARY TABLE
# =============================================================================

cat("\n=============================================================================\n")
cat("SUMMARY: KEY DESCRIPTIVE STATISTICS ACROSS WEIGHTING SCENARIOS\n")
cat("=============================================================================\n")

summary_table <- tibble(
  Statistic = c(
    "Total Contacts (Rep)", "Total Contacts (Dem)", "Contact Gap (R-D)",
    "Mask Usage % (Rep)", "Mask Usage % (Dem)", "Mask Gap (R-D)",
    "Vaccinated % (Rep)", "Vaccinated % (Dem)", "Vaccination Gap (R-D)"
  ),
  Unweighted = c(
    behaviors_unweighted %>% filter(political_party == "Republican") %>% pull(total_contacts),
    behaviors_unweighted %>% filter(political_party == "Democrat") %>% pull(total_contacts),
    gaps_comparison %>% filter(weight_type == "Unweighted") %>% pull(contacts_gap),
    behaviors_unweighted %>% filter(political_party == "Republican") %>% pull(mask_usage),
    behaviors_unweighted %>% filter(political_party == "Democrat") %>% pull(mask_usage),
    gaps_comparison %>% filter(weight_type == "Unweighted") %>% pull(mask_gap),
    vax_unweighted %>% filter(political_party == "Republican") %>% pull(vaccinated),
    vax_unweighted %>% filter(political_party == "Democrat") %>% pull(vaccinated),
    calc_vax_gap(bics_wave6, "none")
  ),
  Original = c(
    behaviors_pooled %>% filter(political_party == "Republican") %>% pull(total_contacts),
    behaviors_pooled %>% filter(political_party == "Democrat") %>% pull(total_contacts),
    gaps_comparison %>% filter(weight_type == "Original (weight_pooled)") %>% pull(contacts_gap),
    behaviors_pooled %>% filter(political_party == "Republican") %>% pull(mask_usage),
    behaviors_pooled %>% filter(political_party == "Democrat") %>% pull(mask_usage),
    gaps_comparison %>% filter(weight_type == "Original (weight_pooled)") %>% pull(mask_gap),
    vax_pooled %>% filter(political_party == "Republican") %>% pull(vaccinated),
    vax_pooled %>% filter(political_party == "Democrat") %>% pull(vaccinated),
    calc_vax_gap(bics_wave6, "weight_pooled")
  ),
  Raked = c(
    behaviors_raked %>% filter(political_party == "Republican") %>% pull(total_contacts),
    behaviors_raked %>% filter(political_party == "Democrat") %>% pull(total_contacts),
    gaps_comparison %>% filter(weight_type == "Raked (weight_party_raked)") %>% pull(contacts_gap),
    behaviors_raked %>% filter(political_party == "Republican") %>% pull(mask_usage),
    behaviors_raked %>% filter(political_party == "Democrat") %>% pull(mask_usage),
    gaps_comparison %>% filter(weight_type == "Raked (weight_party_raked)") %>% pull(mask_gap),
    vax_raked %>% filter(political_party == "Republican") %>% pull(vaccinated),
    vax_raked %>% filter(political_party == "Democrat") %>% pull(vaccinated),
    calc_vax_gap(bics_wave6, "weight_party_raked")
  )
)

summary_table %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = 20)

# Save descriptive summary
write_csv(summary_table, here("out", "tables", "weight_sensitivity_summary.csv"))

# =============================================================================
# PART II: REGRESSION ANALYSIS
# =============================================================================

cat("\n\n")
cat("=============================================================================\n")
cat("REGRESSION WEIGHT SENSITIVITY ANALYSIS\n")
cat("=============================================================================\n")

# =============================================================================
# 7. HELPER FUNCTION TO RUN MODELS WITH DIFFERENT WEIGHTS
# =============================================================================

run_partisan_model <- function(data, outcome_var, predictor_var, weight_var = NULL) {

  # Build formula
  formula <- as.formula(paste(outcome_var, "~", predictor_var))

  # Run model with or without weights
  if (is.null(weight_var) || weight_var == "none") {
    model <- lm(formula, data = data)
  } else {
    model <- lm(formula, data = data, weights = data[[weight_var]])
  }

  # Extract coefficient for the predictor
  coef_summary <- tidy(model) %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate, std.error, statistic, p.value)

  return(coef_summary)
}

run_all_models <- function(data, data_RD, data_ID, outcome_var, weight_var = NULL) {

  # Republican vs Democrat
  rep_dem <- run_partisan_model(data_RD, outcome_var, "republican", weight_var) %>%
    mutate(comparison = "Republican - Democrat")

  # Independent vs Democrat
  ind_dem <- run_partisan_model(data_ID, outcome_var, "Independent", weight_var) %>%
    mutate(comparison = "Independent - Democrat")

  # White vs Non-White
  race <- run_partisan_model(data, outcome_var, "white", weight_var) %>%
    mutate(comparison = "White - Non-White")

  # Male vs Female
  gender <- run_partisan_model(data, outcome_var, "male", weight_var) %>%
    mutate(comparison = "Male - Female")

  # College vs No College
  college <- run_partisan_model(data, outcome_var, "r_college_grad", weight_var) %>%
    mutate(comparison = "College - No College")

  bind_rows(rep_dem, ind_dem, race, gender, college) %>%
    select(comparison, estimate, std.error, p.value)
}

# =============================================================================
# 8. RUN SENSITIVITY ANALYSIS FOR CONTACTS
# =============================================================================

cat("\n=============================================================================\n")
cat("CONTACTS: REGRESSION COEFFICIENTS BY WEIGHTING SCENARIO\n")
cat("=============================================================================\n")

contacts_unweighted <- run_all_models(bics_valid, RD_df, ID_df, "num_cc", "none") %>%
  mutate(weight_type = "Unweighted")

contacts_pooled <- run_all_models(bics_valid, RD_df, ID_df, "num_cc", "weight_pooled") %>%
  mutate(weight_type = "Original")

contacts_raked <- run_all_models(bics_valid, RD_df, ID_df, "num_cc", "weight_party_raked") %>%
  mutate(weight_type = "Raked")

contacts_comparison <- bind_rows(contacts_unweighted, contacts_pooled, contacts_raked) %>%
  select(weight_type, comparison, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Wide format for comparison
contacts_wide <- contacts_comparison %>%
  select(weight_type, comparison, estimate) %>%
  pivot_wider(names_from = weight_type, values_from = estimate)

cat("\nContacts - Coefficient Estimates:\n")
print(contacts_wide)

# =============================================================================
# 9. RUN SENSITIVITY ANALYSIS FOR MASK USAGE
# =============================================================================

cat("\n=============================================================================\n")
cat("MASK USAGE: REGRESSION COEFFICIENTS BY WEIGHTING SCENARIO\n")
cat("=============================================================================\n")

masks_unweighted <- run_all_models(bics_valid, RD_df, ID_df, "Norm_Masks_Used", "none") %>%
  mutate(weight_type = "Unweighted")

masks_pooled <- run_all_models(bics_valid, RD_df, ID_df, "Norm_Masks_Used", "weight_pooled") %>%
  mutate(weight_type = "Original")

masks_raked <- run_all_models(bics_valid, RD_df, ID_df, "Norm_Masks_Used", "weight_party_raked") %>%
  mutate(weight_type = "Raked")

masks_comparison <- bind_rows(masks_unweighted, masks_pooled, masks_raked) %>%
  select(weight_type, comparison, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

masks_wide <- masks_comparison %>%
  select(weight_type, comparison, estimate) %>%
  pivot_wider(names_from = weight_type, values_from = estimate)

cat("\nMask Usage - Coefficient Estimates:\n")
print(masks_wide)

# =============================================================================
# 10. RUN SENSITIVITY ANALYSIS FOR VACCINATION
# =============================================================================

cat("\n=============================================================================\n")
cat("VACCINATION: REGRESSION COEFFICIENTS BY WEIGHTING SCENARIO\n")
cat("=============================================================================\n")

# Filter to wave 6 for vaccination
RD_wave6 <- RD_df %>% filter(wave == 6)
ID_wave6 <- ID_df %>% filter(wave == 6)

vaccines_unweighted <- run_all_models(bics_wave6, RD_wave6, ID_wave6, "Vaccinated", "none") %>%
  mutate(weight_type = "Unweighted")

vaccines_pooled <- run_all_models(bics_wave6, RD_wave6, ID_wave6, "Vaccinated", "weight_pooled") %>%
  mutate(weight_type = "Original")

vaccines_raked <- run_all_models(bics_wave6, RD_wave6, ID_wave6, "Vaccinated", "weight_party_raked") %>%
  mutate(weight_type = "Raked")

vaccines_comparison <- bind_rows(vaccines_unweighted, vaccines_pooled, vaccines_raked) %>%
  select(weight_type, comparison, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 4),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

vaccines_wide <- vaccines_comparison %>%
  select(weight_type, comparison, estimate) %>%
  pivot_wider(names_from = weight_type, values_from = estimate)

cat("\nVaccination - Coefficient Estimates:\n")
print(vaccines_wide)

# =============================================================================
# 11. WAVE-LEVEL PARTISAN DIFFERENCES
# =============================================================================

cat("\n=============================================================================\n")
cat("WAVE-LEVEL PARTISAN GAPS (REPUBLICAN - DEMOCRAT)\n")
cat("=============================================================================\n")

calc_wave_diffs <- function(data, weight_var = NULL) {

  if (is.null(weight_var) || weight_var == "none") {
    data$w <- 1
  } else {
    data$w <- data[[weight_var]]
  }

  # Democrat averages by wave
  dem_wave <- data %>%
    filter(political_party == "Democrat") %>%
    group_by(data_collected_dates) %>%
    summarise(
      dem_contacts = weighted.mean(num_cc, w = w, na.rm = TRUE),
      dem_masks = weighted.mean(Norm_Masks_Used, w = w, na.rm = TRUE),
      .groups = "drop"
    )

  # Republican averages by wave
  rep_wave <- data %>%
    filter(political_party == "Republican") %>%
    group_by(data_collected_dates) %>%
    summarise(
      rep_contacts = weighted.mean(num_cc, w = w, na.rm = TRUE),
      rep_masks = weighted.mean(Norm_Masks_Used, w = w, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate differences
  dem_wave %>%
    inner_join(rep_wave, by = "data_collected_dates") %>%
    mutate(
      contacts_diff = rep_contacts - dem_contacts,
      masks_diff = rep_masks - dem_masks
    ) %>%
    select(data_collected_dates, contacts_diff, masks_diff)
}

wave_diffs_unweighted <- calc_wave_diffs(bics_valid, "none") %>%
  mutate(weight_type = "Unweighted")

wave_diffs_pooled <- calc_wave_diffs(bics_valid, "weight_pooled") %>%
  mutate(weight_type = "Original")

wave_diffs_raked <- calc_wave_diffs(bics_valid, "weight_party_raked") %>%
  mutate(weight_type = "Raked")

wave_diffs_all <- bind_rows(wave_diffs_unweighted, wave_diffs_pooled, wave_diffs_raked)

cat("\nContact Differences by Wave (Rep - Dem):\n")
wave_diffs_all %>%
  select(weight_type, data_collected_dates, contacts_diff) %>%
  pivot_wider(names_from = weight_type, values_from = contacts_diff) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print()

cat("\nMask Usage Differences by Wave (Rep - Dem):\n")
wave_diffs_all %>%
  select(weight_type, data_collected_dates, masks_diff) %>%
  pivot_wider(names_from = weight_type, values_from = masks_diff) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

# =============================================================================
# 12. REGRESSION SUMMARY TABLE
# =============================================================================

cat("\n=============================================================================\n")
cat("SUMMARY: PARTISAN REGRESSION COEFFICIENTS ACROSS WEIGHTING SCENARIOS\n")
cat("=============================================================================\n")

regression_summary <- bind_rows(
  contacts_comparison %>% mutate(outcome = "Contacts"),
  masks_comparison %>% mutate(outcome = "Mask Usage"),
  vaccines_comparison %>% mutate(outcome = "Vaccination")
) %>%
  filter(comparison %in% c("Republican - Democrat", "Independent - Democrat")) %>%
  select(outcome, comparison, weight_type, estimate, std.error, sig) %>%
  arrange(outcome, comparison, weight_type)

cat("\n")
print(regression_summary, n = 30)

# Create wide format for easy comparison
regression_wide <- regression_summary %>%
  mutate(est_se = paste0(estimate, " (", std.error, ")", sig)) %>%
  select(outcome, comparison, weight_type, est_se) %>%
  pivot_wider(names_from = weight_type, values_from = est_se)

cat("\nFormatted Summary (Estimate (SE) Significance):\n")
print(regression_wide)

# =============================================================================
# 13. SAVE ALL RESULTS
# =============================================================================

# Combine all regression comparisons
all_regression_results <- bind_rows(
  contacts_comparison %>% mutate(outcome = "Contacts"),
  masks_comparison %>% mutate(outcome = "Mask Usage"),
  vaccines_comparison %>% mutate(outcome = "Vaccination")
)

write_csv(all_regression_results, here("out", "tables", "regression_weight_sensitivity.csv"))

cat("\n=============================================================================\n")
cat("Sensitivity analysis complete!\n")
cat("Results saved to:\n")
cat("  - out/tables/weight_sensitivity_summary.csv (descriptive)\n")
cat("  - out/tables/regression_weight_sensitivity.csv (regression)\n")
cat("=============================================================================\n")
