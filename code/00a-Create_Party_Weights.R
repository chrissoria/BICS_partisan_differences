# =============================================================================
# Create Party-Based Sample Weights Using Raking
# =============================================================================
# This script uses raking (iterative proportional fitting) to adjust the BICS
# sample weights to match national party identification distributions from
# Gallup (2020), while preserving the existing demographic calibration.
#
# Target distribution (Gallup 2020):
#   - Democrat: 30%
#   - Republican: 29%
#   - Independent: 39%
#
# Reference: Gallup (2021). Party ID Average for 2020 Winds Up Similar to Prior Years.
# https://news.gallup.com/poll/328310/party-average-2020-winds-similar-prior-years.aspx
#
# Why raking instead of simple multiplication?
# Party ID is correlated with demographics (age, race, gender, education).
# The existing weight_pooled already adjusts for these demographics.
# Simple multiplication would "double count" some adjustments.
# Raking jointly calibrates to both the existing weighted demographics AND
# party targets, finding weights that satisfy all constraints.
# =============================================================================

library(tidyverse)
library(survey)
library(here)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Load the BICS data
if (!exists("bics_zip_features")) {
  data_path <- here("data", "BICS_ZIP_Features.csv")
  if (file.exists(data_path)) {
    bics_zip_features <- read_csv(data_path)
    cat("Loaded data from:", data_path, "\n")
  } else {
    stop("Data file not found. Please run 00-Prep_BICS_Data.Rmd first.")
  }
}

# =============================================================================
# 2. DEFINE TARGET PROPORTIONS (GALLUP 2020)
# =============================================================================

# National party identification targets from Gallup 2020 averages
# Original: Democrat 30%, Republican 29%, Independent 39%, Other 2%
# We normalize to the three parties we have in our sample

gallup_raw <- c(Democrat = 0.30, Republican = 0.29, Independent = 0.39)
gallup_targets <- gallup_raw / sum(gallup_raw)  # Normalize to sum to 1

cat("\n=============================================================================\n")
cat("TARGET PROPORTIONS (Gallup 2020, normalized)\n")
cat("=============================================================================\n")
print(round(gallup_targets, 4))

# =============================================================================
# 3. PREPARE DATA FOR RAKING
# =============================================================================

# Filter to valid party responses only
valid_parties <- c("Democrat", "Republican", "Independent")

bics_for_raking <- bics_zip_features %>%
  filter(political_party %in% valid_parties) %>%
  mutate(
    # Ensure political_party is a factor with correct levels
    political_party = factor(political_party, levels = valid_parties)
  )

n_valid <- nrow(bics_for_raking)
cat("\nSample size for raking:", n_valid, "\n")

# =============================================================================
# 4. CALCULATE CURRENT SAMPLE PROPORTIONS
# =============================================================================

# Unweighted proportions
unweighted_props <- bics_for_raking %>%
  count(political_party) %>%
  mutate(unweighted_prop = n / sum(n))

# Currently weighted proportions (using weight_pooled)
weighted_props <- bics_for_raking %>%
  group_by(political_party) %>%
  summarise(weighted_n = sum(weight_pooled), .groups = "drop") %>%
  mutate(current_weighted_prop = weighted_n / sum(weighted_n))

current_props <- unweighted_props %>%
  left_join(weighted_props, by = "political_party") %>%
  mutate(target_prop = gallup_targets[as.character(political_party)])

cat("\n=============================================================================\n")
cat("CURRENT VS TARGET PROPORTIONS\n")
cat("=============================================================================\n")
print(current_props %>%
        select(political_party, n, unweighted_prop, current_weighted_prop, target_prop) %>%
        mutate(across(where(is.numeric) & !matches("^n$"), ~round(., 4))))

# =============================================================================
# 5. CREATE SURVEY DESIGN AND RAKE
# =============================================================================

cat("\n=============================================================================\n")
cat("RAKING TO PARTY ID TARGETS\n")
cat("=============================================================================\n")

# Create survey design with existing pooled weights
svy_design <- svydesign(
  ids = ~1,
  weights = ~weight_pooled,
  data = bics_for_raking
)

# Define population margins for party ID
# The margins should be counts (proportions * sample size)
party_pop_margins <- data.frame(
  political_party = factor(valid_parties, levels = valid_parties),
  Freq = gallup_targets * n_valid
)

cat("\nTarget population margins:\n")
print(party_pop_margins)

# Perform raking
svy_raked <- rake(
  design = svy_design,
  sample.margins = list(~political_party),
  population.margins = list(party_pop_margins),
  control = list(maxit = 50, epsilon = 1e-6)
)

cat("\nRaking converged successfully.\n")

# =============================================================================
# 6. EXTRACT AND EVALUATE RAKED WEIGHTS
# =============================================================================

# Extract the raked weights
bics_for_raking$weight_party_raked <- weights(svy_raked)

# Verify the raked proportions match targets
raked_props <- bics_for_raking %>%
  group_by(political_party) %>%
  summarise(
    raked_weighted_n = sum(weight_party_raked),
    .groups = "drop"
  ) %>%
  mutate(raked_prop = raked_weighted_n / sum(raked_weighted_n))

verification <- current_props %>%
  left_join(raked_props %>% select(political_party, raked_prop),
            by = "political_party")

cat("\n=============================================================================\n")
cat("VERIFICATION: RAKED PROPORTIONS VS TARGETS\n")
cat("=============================================================================\n")
print(verification %>%
        select(political_party, current_weighted_prop, raked_prop, target_prop) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# =============================================================================
# 7. WEIGHT DIAGNOSTICS
# =============================================================================

cat("\n=============================================================================\n")
cat("WEIGHT DIAGNOSTICS\n")
cat("=============================================================================\n")

# Compare original vs raked weights
weight_comparison <- bics_for_raking %>%
  group_by(political_party) %>%
  summarise(
    n = n(),
    orig_mean = mean(weight_pooled),
    raked_mean = mean(weight_party_raked),
    adjustment_ratio = mean(weight_party_raked) / mean(weight_pooled),
    orig_cv = sd(weight_pooled) / mean(weight_pooled),
    raked_cv = sd(weight_party_raked) / mean(weight_party_raked),
    .groups = "drop"
  )

cat("\nWeight adjustment by party:\n")
print(weight_comparison %>% mutate(across(where(is.numeric) & !matches("^n$"), ~round(., 4))))

# Overall weight statistics
cat("\nOverall weight statistics:\n")
cat(sprintf("  Original weights (weight_pooled):\n"))
cat(sprintf("    Mean: %.4f, SD: %.4f, Min: %.4f, Max: %.4f\n",
            mean(bics_for_raking$weight_pooled),
            sd(bics_for_raking$weight_pooled),
            min(bics_for_raking$weight_pooled),
            max(bics_for_raking$weight_pooled)))

cat(sprintf("  Raked weights (weight_party_raked):\n"))
cat(sprintf("    Mean: %.4f, SD: %.4f, Min: %.4f, Max: %.4f\n",
            mean(bics_for_raking$weight_party_raked),
            sd(bics_for_raking$weight_party_raked),
            min(bics_for_raking$weight_party_raked),
            max(bics_for_raking$weight_party_raked)))

# Design effect
cat(sprintf("\n  Design effect (original): %.2f\n",
            1 + (sd(bics_for_raking$weight_pooled)/mean(bics_for_raking$weight_pooled))^2))
cat(sprintf("  Design effect (raked): %.2f\n",
            1 + (sd(bics_for_raking$weight_party_raked)/mean(bics_for_raking$weight_party_raked))^2))

# =============================================================================
# 8. MERGE RAKED WEIGHTS BACK TO FULL DATASET
# =============================================================================

# Create a unique identifier if not present
if (!"row_id" %in% names(bics_zip_features)) {
  bics_zip_features$row_id <- 1:nrow(bics_zip_features)
  bics_for_raking$row_id <- which(bics_zip_features$political_party %in% valid_parties)
}

# Merge raked weights back
bics_zip_features <- bics_zip_features %>%
  left_join(
    bics_for_raking %>% select(row_id, weight_party_raked),
    by = "row_id"
  ) %>%
  mutate(
    # For non-valid parties (e.g., "Prefer not to answer"), keep original weight
    weight_party_raked = coalesce(weight_party_raked, weight_pooled)
  ) %>%
  select(-row_id)

# Also remove row_id from bics_for_raking
bics_for_raking <- bics_for_raking %>% select(-row_id)

# =============================================================================
# 9. SAVE UPDATED DATA (OPTIONAL)
# =============================================================================

# Uncomment to save
# write_csv(bics_zip_features, here("data", "BICS_ZIP_Features_PartyWeighted.csv"))

cat("\n=============================================================================\n")
cat("RAKING COMPLETE\n")
cat("=============================================================================\n")
cat("New variable added to bics_zip_features:\n")
cat("  - weight_party_raked: Raked weights calibrated to Gallup party ID targets\n")
cat("\n")
cat("Usage: Replace weight_pooled with weight_party_raked in analyses, e.g.:\n")
cat("  weighted.mean(x, w = weight_party_raked)\n")
cat("  svydesign(ids = ~1, weights = ~weight_party_raked, data = df)\n")
cat("=============================================================================\n")
