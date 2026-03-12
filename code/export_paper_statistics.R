# =============================================================================
# Export Paper Statistics
# =============================================================================
# This script calculates and exports all key statistics needed for the paper's
# results section. Run this after 00-Prep_BICS_Data.Rmd to generate the
# statistics file that the paper reads.
#
# Output: out/tables/paper_statistics.rds
# =============================================================================

library(tidyverse)
library(here)
library(purrr)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

cat("Loading data...\n")

# Load the BICS data
data_path <- here("data", "BICS_ZIP_Features.csv")
if (!file.exists(data_path)) {
  stop("Data file not found. Please run 00-Prep_BICS_Data.Rmd first.")
}

bics <- read_csv(data_path, show_col_types = FALSE)

# Use weight_party_raked if available, otherwise fall back to weight_pooled
if (!"weight_party_raked" %in% names(bics)) {
  bics$weight_party_raked <- bics$weight_pooled
  cat("Note: weight_party_raked not found, using weight_pooled.\n")
}

# Filter to valid parties
valid_parties <- c("Democrat", "Republican", "Independent")
bics_valid <- bics %>%
  filter(political_party %in% valid_parties)

# Create vax_acceptance variable (vaccinated OR intends to vaccinate)
# This variable is created in data prep but not exported to CSV
bics_valid <- bics_valid %>%
  mutate(
    vax_acceptance = case_when(
      Vaccinated == 1 ~ 1L,
      will_get_vax == 1 ~ 1L,
      !is.na(Vaccinated) | !is.na(will_get_vax) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Create census region variable
northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire",
               "New Jersey", "New York", "Pennsylvania", "Rhode Island", "Vermont")
midwest <- c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota",
             "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
south <- c("Alabama", "Arkansas", "Delaware", "District of Columbia", "Florida",
           "Georgia", "Kentucky", "Louisiana", "Maryland", "Mississippi",
           "North Carolina", "Oklahoma", "South Carolina", "Tennessee", "Texas",
           "Virginia", "West Virginia")
west <- c("Alaska", "Arizona", "California", "Colorado", "Hawaii", "Idaho",
          "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming")

# Create census division variable (9 divisions)
new_england <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
middle_atlantic <- c("New Jersey", "New York", "Pennsylvania")
east_north_central <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
west_north_central <- c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
south_atlantic <- c("Delaware", "District of Columbia", "Florida", "Georgia", "Maryland",
                    "North Carolina", "South Carolina", "Virginia", "West Virginia")
east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west_south_central <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
mountain <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming")
pacific <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")

bics_valid <- bics_valid %>%
  mutate(
    census_region = case_when(
      STATE %in% northeast ~ "Northeast",
      STATE %in% midwest ~ "Midwest",
      STATE %in% south ~ "South",
      STATE %in% west ~ "West",
      TRUE ~ NA_character_
    ),
    census_region = factor(census_region, levels = c("Northeast", "Midwest", "South", "West")),
    census_division = case_when(
      STATE %in% new_england ~ "New England",
      STATE %in% middle_atlantic ~ "Middle Atlantic",
      STATE %in% east_north_central ~ "East North Central",
      STATE %in% west_north_central ~ "West North Central",
      STATE %in% south_atlantic ~ "South Atlantic",
      STATE %in% east_south_central ~ "East South Central",
      STATE %in% west_south_central ~ "West South Central",
      STATE %in% mountain ~ "Mountain",
      STATE %in% pacific ~ "Pacific",
      TRUE ~ NA_character_
    ),
    census_division = factor(census_division, levels = c(
      "New England", "Middle Atlantic", "East North Central", "West North Central",
      "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"
    ))
  )

cat("Sample size:", nrow(bics_valid), "\n\n")

# =============================================================================
# 2. SAMPLE CHARACTERISTICS
# =============================================================================

cat("Calculating sample characteristics...\n")

# Sample sizes
n_total <- nrow(bics_valid)
n_dem <- sum(bics_valid$political_party == "Democrat")
n_rep <- sum(bics_valid$political_party == "Republican")
n_ind <- sum(bics_valid$political_party == "Independent")

# Demographics by party (weighted)
demo_by_party <- bics_valid %>%
  group_by(political_party) %>%
  summarise(
    avg_age = weighted.mean(age, w = weight_party_raked, na.rm = TRUE),
    pct_male = weighted.mean(male, w = weight_party_raked, na.rm = TRUE) * 100,
    pct_white = weighted.mean(white, w = weight_party_raked, na.rm = TRUE) * 100,
    pct_metro = weighted.mean(Metro, w = weight_party_raked, na.rm = TRUE) * 100,
    pct_college = weighted.mean(as.numeric(r_college_grad), w = weight_party_raked, na.rm = TRUE) * 100,
    pct_employed = weighted.mean(r_working_num, w = weight_party_raked, na.rm = TRUE) * 100,
    avg_hhsize = weighted.mean(resp_hhsize, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 3. HEALTH BEHAVIORS
# =============================================================================

cat("Calculating health behaviors...\n")

behaviors_by_party <- bics_valid %>%
  group_by(political_party) %>%
  summarise(
    total_contacts = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    # Norm_Masks_Used is already on 0-100 scale, don't multiply
    mask_usage = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 3a. PANDEMIC PERCEPTIONS
# =============================================================================

cat("Calculating pandemic perceptions...\n")

# Concern about pandemic by party
# binary_concern_strong = 1 if "Very concerned" about COVID
perceptions_by_party <- bics_valid %>%
  group_by(political_party) %>%
  summarise(
    pct_very_concerned = weighted.mean(binary_concern_strong, w = weight_party_raked, na.rm = TRUE) * 100,
    pct_any_concern = weighted.mean(binary_concern, w = weight_party_raked, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Extract values
concern_dem <- perceptions_by_party %>% filter(political_party == "Democrat") %>% pull(pct_very_concerned)
concern_rep <- perceptions_by_party %>% filter(political_party == "Republican") %>% pull(pct_very_concerned)
concern_ind <- perceptions_by_party %>% filter(political_party == "Independent") %>% pull(pct_very_concerned)

any_concern_dem <- perceptions_by_party %>% filter(political_party == "Democrat") %>% pull(pct_any_concern)
any_concern_rep <- perceptions_by_party %>% filter(political_party == "Republican") %>% pull(pct_any_concern)
any_concern_ind <- perceptions_by_party %>% filter(political_party == "Independent") %>% pull(pct_any_concern)

concern_gap <- concern_dem - concern_rep

# Perception by wave for appendix figure
perceptions_by_wave <- bics_valid %>%
  group_by(wave, political_party) %>%
  summarise(
    pct_very_concerned = weighted.mean(binary_concern_strong, w = weight_party_raked, na.rm = TRUE) * 100,
    pct_any_concern = weighted.mean(binary_concern, w = weight_party_raked, na.rm = TRUE) * 100,
    n = n(),
    .groups = "drop"
  )

# Calculate gaps by wave
perception_gaps_by_wave <- perceptions_by_wave %>%
  select(wave, political_party, pct_very_concerned) %>%
  pivot_wider(names_from = political_party, values_from = pct_very_concerned) %>%
  mutate(
    concern_gap = Democrat - Republican,
    concern_gap_ind = Democrat - Independent
  )

# Export perception by wave
write_csv(perceptions_by_wave, "out/tables/perceptions_by_wave.csv")
write_csv(perception_gaps_by_wave, "out/tables/perception_gaps_by_wave.csv")

# Calculate average LOCAL incidence experienced by each party, by wave
# This shows the actual pandemic conditions each party faced (given where they live)
cat("Calculating local incidence experienced by party and wave...\n")

local_conditions_by_party_wave <- bics_valid %>%
  filter(!is.na(prev_week_inc_rate), !is.na(prev_week_mort_rate)) %>%
  group_by(wave, political_party) %>%
  summarise(
    mean_local_incidence = weighted.mean(prev_week_inc_rate, w = weight_party_raked, na.rm = TRUE),
    mean_local_mortality = weighted.mean(prev_week_mort_rate, w = weight_party_raked, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Join with perceptions to create combined dataset for plotting
perceptions_with_local_conditions <- perceptions_by_wave %>%
  left_join(local_conditions_by_party_wave, by = c("wave", "political_party"))

write_csv(perceptions_with_local_conditions, "out/tables/perceptions_with_local_conditions.csv")

cat("  Local conditions by party saved to out/tables/perceptions_with_local_conditions.csv\n")

# =============================================================================
# 3b. BEHAVIORS BY CONCERN LEVEL AND PARTY
# =============================================================================
# Test whether partisan gaps persist within each level of stated concern
# If concern explains the gap, behaviors should be similar within each concern level

cat("Calculating behaviors by concern level and party...\n")

# Calculate behaviors by party within each concern level
behaviors_by_concern <- bics_valid %>%
  filter(!is.na(covid19_concern)) %>%
  mutate(
    concern_level = factor(covid19_concern,
                           levels = c("Not at all concerned", "Not very concerned",
                                      "Somewhat concerned", "Very concerned"))
  ) %>%
  group_by(concern_level, political_party) %>%
  summarise(
    contacts = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    mask_usage = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Export for plotting
write_csv(behaviors_by_concern, "out/tables/behaviors_by_concern_level.csv")

# Extract mask usage by party at each concern level for inline stats
mask_by_concern_dem <- behaviors_by_concern %>%
  filter(political_party == "Democrat") %>%
  arrange(concern_level)
mask_by_concern_rep <- behaviors_by_concern %>%
  filter(political_party == "Republican") %>%
  arrange(concern_level)

# Mask usage at each concern level
mask_notconcerned_dem <- mask_by_concern_dem %>% filter(concern_level == "Not at all concerned") %>% pull(mask_usage)
mask_notconcerned_rep <- mask_by_concern_rep %>% filter(concern_level == "Not at all concerned") %>% pull(mask_usage)
mask_notvery_dem <- mask_by_concern_dem %>% filter(concern_level == "Not very concerned") %>% pull(mask_usage)
mask_notvery_rep <- mask_by_concern_rep %>% filter(concern_level == "Not very concerned") %>% pull(mask_usage)
mask_somewhat_dem <- mask_by_concern_dem %>% filter(concern_level == "Somewhat concerned") %>% pull(mask_usage)
mask_somewhat_rep <- mask_by_concern_rep %>% filter(concern_level == "Somewhat concerned") %>% pull(mask_usage)
mask_veryconcerned_dem <- mask_by_concern_dem %>% filter(concern_level == "Very concerned") %>% pull(mask_usage)
mask_veryconcerned_rep <- mask_by_concern_rep %>% filter(concern_level == "Very concerned") %>% pull(mask_usage)

# Calculate slopes (change from "Not at all concerned" to "Very concerned")
mask_slope_dem <- mask_veryconcerned_dem - mask_notconcerned_dem
mask_slope_rep <- mask_veryconcerned_rep - mask_notconcerned_rep

cat(sprintf("  Mask slopes: Democrats = %.1f pp, Republicans = %.1f pp\n", mask_slope_dem, mask_slope_rep))

# Also calculate for Wave 1 only (when mortality was highest)
# Note: Using unweighted means since weights were designed for full sample, not wave-specific subsamples
# Using binary concern (Very concerned vs. not) for more statistical power with small sample
behaviors_by_concern_wave1 <- bics_valid %>%
  filter(!is.na(binary_concern_strong), wave == 1) %>%
  mutate(
    concern_level = factor(
      ifelse(binary_concern_strong == 1, "Very Concerned", "Less Concerned"),
      levels = c("Less Concerned", "Very Concerned")
    )
  ) %>%
  group_by(concern_level, political_party) %>%
  summarise(
    contacts = mean(num_cc, na.rm = TRUE),
    nonhh_contacts = mean(num_cc_nonhh, na.rm = TRUE),
    mask_usage = mean(Norm_Masks_Used, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write_csv(behaviors_by_concern_wave1, "out/tables/behaviors_by_concern_level_wave1.csv")
cat("  Wave 1 behaviors by concern binary (unweighted) exported to behaviors_by_concern_level_wave1.csv\n")

# Calculate mask usage among "very concerned" by wave to show politicization over time
# Using unweighted means for consistency with wave 1 analysis
mask_among_very_concerned_by_wave <- bics_valid %>%
  filter(binary_concern_strong == 1) %>%
  group_by(wave, political_party) %>%
  summarise(
    mask_usage = mean(Norm_Masks_Used, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

write_csv(mask_among_very_concerned_by_wave, "out/tables/mask_among_very_concerned_by_wave.csv")
cat("  Mask usage among very concerned by wave exported to mask_among_very_concerned_by_wave.csv\n")

# Also calculate behaviors among only "very concerned" for inline stats
bics_very_concerned <- bics_valid %>%
  filter(covid19_concern == "Very concerned")

behaviors_very_concerned <- bics_very_concerned %>%
  group_by(political_party) %>%
  summarise(
    contacts_vc = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts_vc = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    mask_usage_vc = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    n_vc = n(),
    .groups = "drop"
  )

# Extract values for very concerned
contacts_vc_dem <- behaviors_very_concerned %>% filter(political_party == "Democrat") %>% pull(contacts_vc)
contacts_vc_rep <- behaviors_very_concerned %>% filter(political_party == "Republican") %>% pull(contacts_vc)
contacts_vc_ind <- behaviors_very_concerned %>% filter(political_party == "Independent") %>% pull(contacts_vc)

nonhh_vc_dem <- behaviors_very_concerned %>% filter(political_party == "Democrat") %>% pull(nonhh_contacts_vc)
nonhh_vc_rep <- behaviors_very_concerned %>% filter(political_party == "Republican") %>% pull(nonhh_contacts_vc)
nonhh_vc_ind <- behaviors_very_concerned %>% filter(political_party == "Independent") %>% pull(nonhh_contacts_vc)

mask_vc_dem <- behaviors_very_concerned %>% filter(political_party == "Democrat") %>% pull(mask_usage_vc)
mask_vc_rep <- behaviors_very_concerned %>% filter(political_party == "Republican") %>% pull(mask_usage_vc)
mask_vc_ind <- behaviors_very_concerned %>% filter(political_party == "Independent") %>% pull(mask_usage_vc)

n_vc_dem <- behaviors_very_concerned %>% filter(political_party == "Democrat") %>% pull(n_vc)
n_vc_rep <- behaviors_very_concerned %>% filter(political_party == "Republican") %>% pull(n_vc)
n_vc_ind <- behaviors_very_concerned %>% filter(political_party == "Independent") %>% pull(n_vc)

# Gaps among the very concerned
contacts_vc_gap <- contacts_vc_rep - contacts_vc_dem
nonhh_vc_gap <- nonhh_vc_rep - nonhh_vc_dem
mask_vc_gap <- mask_vc_dem - mask_vc_rep  # D-R for masks

cat(sprintf("  Among 'very concerned': R-D contact gap = %.2f, D-R mask gap = %.1f pp\n",
            contacts_vc_gap, mask_vc_gap))

# =============================================================================
# 3c. INTERACTION MODELS: CONCERN AND MORTALITY AS MODERATORS OF PARTISAN GAP
# =============================================================================
# Model: Behavior = party + concern + mortality + party*concern + party*mortality + controls
# This tests whether concern and/or mortality moderate the partisan gap

cat("Calculating interaction models for concern/mortality moderators...\n")

library(broom)

# Prepare data for interaction models (R and D only)
model_data <- bics_valid %>%
  filter(political_party %in% c("Democrat", "Republican")) %>%
  filter(!is.na(num_cc), !is.na(Norm_Masks_Used), !is.na(binary_concern_strong),
         !is.na(prev_week_mort_rate), !is.na(Metro), !is.na(wave)) %>%
  mutate(
    republican = ifelse(political_party == "Republican", 1, 0),
    very_concerned = binary_concern_strong,
    log_mort_std = scale(log(prev_week_mort_rate + 0.001))[,1],
    wave = factor(wave)
  )

# Model for CONTACTS
contact_int_model <- lm(num_cc ~ republican * very_concerned + republican * log_mort_std +
                          wave + Metro,
                        data = model_data,
                        weights = weight_party_raked)

# Model for MASKS
mask_int_model <- lm(Norm_Masks_Used ~ republican * very_concerned + republican * log_mort_std +
                       wave + Metro,
                     data = model_data,
                     weights = weight_party_raked)

# Extract coefficients
contact_int_coefs <- tidy(contact_int_model) %>%
  filter(term %in% c("republican", "very_concerned", "log_mort_std",
                     "republican:very_concerned", "republican:log_mort_std"))

mask_int_coefs <- tidy(mask_int_model) %>%
  filter(term %in% c("republican", "very_concerned", "log_mort_std",
                     "republican:very_concerned", "republican:log_mort_std"))

# Create summary table for export
interaction_results <- bind_rows(
  contact_int_coefs %>% mutate(outcome = "Contacts"),
  mask_int_coefs %>% mutate(outcome = "Masks")
)

write_csv(interaction_results, "out/tables/interaction_model_results.csv")

# Extract key values for paper_stats
contact_int_baseline <- contact_int_coefs %>% filter(term == "republican") %>% pull(estimate)
contact_int_concern <- contact_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(estimate)
contact_int_mort <- contact_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(estimate)
contact_int_concern_se <- contact_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(std.error)
contact_int_mort_se <- contact_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(std.error)
contact_int_concern_p <- contact_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(p.value)
contact_int_mort_p <- contact_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(p.value)

mask_int_baseline <- mask_int_coefs %>% filter(term == "republican") %>% pull(estimate)
mask_int_concern <- mask_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(estimate)
mask_int_mort <- mask_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(estimate)
mask_int_concern_se <- mask_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(std.error)
mask_int_mort_se <- mask_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(std.error)
mask_int_concern_p <- mask_int_coefs %>% filter(term == "republican:very_concerned") %>% pull(p.value)
mask_int_mort_p <- mask_int_coefs %>% filter(term == "republican:log_mort_std") %>% pull(p.value)

cat(sprintf("  Wave FE - Contacts: baseline gap = %.2f, concern int = %.2f (p=%.3f), mort int = %.2f (p=%.3f)\n",
            contact_int_baseline, contact_int_concern, contact_int_concern_p,
            contact_int_mort, contact_int_mort_p))
cat(sprintf("  Wave FE - Masks: baseline gap = %.1f, concern int = %.1f (p=%.3f), mort int = %.1f (p=%.3f)\n",
            mask_int_baseline, mask_int_concern, mask_int_concern_p,
            mask_int_mort, mask_int_mort_p))

# -----------------------------------------------------------------------------
# COUNTY FIXED EFFECTS MODEL (local variation)
# This model asks: within the same county, when local mortality rises,
# does the partisan gap narrow?
# -----------------------------------------------------------------------------

cat("Calculating county FE interaction models (local variation)...\n")

# Prepare data with county FE - need counties with sufficient observations
model_data_county <- model_data %>%
  filter(!is.na(COUNTY_FIPS)) %>%
  group_by(COUNTY_FIPS) %>%
  filter(n() >= 10) %>%  # Counties with at least 10 observations
  ungroup() %>%
  mutate(county = factor(COUNTY_FIPS))

cat(sprintf("  Counties with 10+ obs: %d, total N: %d\n",
            n_distinct(model_data_county$county), nrow(model_data_county)))

# Model for CONTACTS with county FE
contact_county_model <- lm(num_cc ~ republican * very_concerned + republican * log_mort_std + county,
                           data = model_data_county,
                           weights = weight_party_raked)

# Model for MASKS with county FE
mask_county_model <- lm(Norm_Masks_Used ~ republican * very_concerned + republican * log_mort_std + county,
                        data = model_data_county,
                        weights = weight_party_raked)

# Extract coefficients (only the key terms, not the county FEs)
contact_county_coefs <- tidy(contact_county_model) %>%
  filter(term %in% c("republican", "very_concerned", "log_mort_std",
                     "republican:very_concerned", "republican:log_mort_std"))

mask_county_coefs <- tidy(mask_county_model) %>%
  filter(term %in% c("republican", "very_concerned", "log_mort_std",
                     "republican:very_concerned", "republican:log_mort_std"))

# Create summary table for export
interaction_county_results <- bind_rows(
  contact_county_coefs %>% mutate(outcome = "Contacts", model = "County FE"),
  mask_county_coefs %>% mutate(outcome = "Masks", model = "County FE")
)

# Add model label to wave FE results and combine
interaction_results <- interaction_results %>%
  mutate(model = "Wave FE")

interaction_all_results <- bind_rows(interaction_results, interaction_county_results)

write_csv(interaction_all_results, "out/tables/interaction_model_results.csv")

# Extract key values for county FE model
contact_county_baseline <- contact_county_coefs %>% filter(term == "republican") %>% pull(estimate)
contact_county_concern <- contact_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(estimate)
contact_county_mort <- contact_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(estimate)
contact_county_concern_se <- contact_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(std.error)
contact_county_mort_se <- contact_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(std.error)
contact_county_concern_p <- contact_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(p.value)
contact_county_mort_p <- contact_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(p.value)

mask_county_baseline <- mask_county_coefs %>% filter(term == "republican") %>% pull(estimate)
mask_county_concern <- mask_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(estimate)
mask_county_mort <- mask_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(estimate)
mask_county_concern_se <- mask_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(std.error)
mask_county_mort_se <- mask_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(std.error)
mask_county_concern_p <- mask_county_coefs %>% filter(term == "republican:very_concerned") %>% pull(p.value)
mask_county_mort_p <- mask_county_coefs %>% filter(term == "republican:log_mort_std") %>% pull(p.value)

cat(sprintf("  County FE - Contacts: baseline gap = %.2f, concern int = %.2f (p=%.3f), mort int = %.2f (p=%.3f)\n",
            contact_county_baseline, contact_county_concern, contact_county_concern_p,
            contact_county_mort, contact_county_mort_p))
cat(sprintf("  County FE - Masks: baseline gap = %.1f, concern int = %.1f (p=%.3f), mort int = %.1f (p=%.3f)\n",
            mask_county_baseline, mask_county_concern, mask_county_concern_p,
            mask_county_mort, mask_county_mort_p))

# =============================================================================
# 4. VACCINE ACCEPTANCE (Wave 6)
# =============================================================================
# vax_acceptance combines: already vaccinated OR intend to get vaccinated
# This captures vaccine acceptance regardless of access timing

cat("Calculating vaccine acceptance statistics...\n")

bics_wave6 <- bics_valid %>% filter(wave == 6)

# Calculate vaccine acceptance rates by party
vax_by_party <- bics_wave6 %>%
  group_by(political_party) %>%
  summarise(
    pct_vaccinated = weighted.mean(vax_acceptance, w = weight_party_raked, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Vaccination intentions among unvaccinated (keep for reference)
bics_unvax <- bics_wave6 %>% filter(Vaccinated == 0)

if ("will_get_vax" %in% names(bics_unvax)) {
  vax_intent_by_party <- bics_unvax %>%
    group_by(political_party) %>%
    summarise(
      pct_will_vax = weighted.mean(will_get_vax, w = weight_party_raked, na.rm = TRUE) * 100,
      pct_wont_vax = weighted.mean(will_not_get_vax, w = weight_party_raked, na.rm = TRUE) * 100,
      .groups = "drop"
    )
} else {
  vax_intent_by_party <- tibble(
    political_party = valid_parties,
    pct_will_vax = NA_real_,
    pct_wont_vax = NA_real_
  )
}

# =============================================================================
# 5. BEHAVIORS BY WAVE (for time trends)
# =============================================================================

cat("Calculating wave-level statistics...\n")

behaviors_by_wave <- bics_valid %>%
  group_by(political_party, wave, data_collected_dates) %>%
  summarise(
    n = n(),
    total_contacts = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    mask_usage = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    political_party = factor(political_party, levels = c("Democrat", "Independent", "Republican"))
  ) %>%
  arrange(wave, political_party)

# Calculate national mortality context per wave
mortality_by_wave <- bics_valid %>%
  group_by(wave, data_collected_dates) %>%
  summarise(
    # Use weighted mean of previous week mortality rate
    mean_mort_rate = weighted.mean(prev_week_mort_rate, w = weight_party_raked, na.rm = TRUE),
    log_mean_mort = weighted.mean(log_prev_week_mort_rate, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Categorize mortality levels
    mortality_level = case_when(
      mean_mort_rate < 0.5 ~ "Low",
      mean_mort_rate < 1.5 ~ "Moderate",
      TRUE ~ "High"
    ),
    mortality_level = factor(mortality_level, levels = c("Low", "Moderate", "High"))
  )

# Calculate partisan gaps by wave with mortality context
gaps_by_wave <- behaviors_by_wave %>%
  select(political_party, wave, data_collected_dates, total_contacts, mask_usage) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(total_contacts, mask_usage)
  ) %>%
  left_join(mortality_by_wave, by = c("wave", "data_collected_dates")) %>%
  mutate(
    contacts_gap = total_contacts_Republican - total_contacts_Democrat,
    mask_gap = mask_usage_Democrat - mask_usage_Republican
  )

# =============================================================================
# 6. PANDEMIC RESPONSE REGRESSION MODELS
# =============================================================================
# These models examine how partisans respond to changes in incidence/mortality
# rates over time. We include county fixed effects so the interpretation is:
# "within the same county, as incidence changes over time, how do partisans
# respond?" This isolates temporal variation from geographic variation.
# We omit wave fixed effects to avoid absorbing the temporal variation we
# want to study.
#
# We standardize (z-score) both log incidence and log mortality so that
# coefficients are directly comparable: effect of 1 SD increase in incidence
# vs 1 SD increase in mortality.
# =============================================================================

cat("Calculating pandemic response models...\n")

library(broom)

# Standardize log incidence and log mortality for comparability
bics_valid <- bics_valid %>%
  mutate(
    log_inc_std = scale(log_prev_week_inc_rate)[,1],
    log_mort_std = scale(log_prev_week_mort_rate)[,1]
  )

# Store SDs for interpretation
sd_log_inc <- sd(bics_valid$log_prev_week_inc_rate, na.rm = TRUE)
sd_log_mort <- sd(bics_valid$log_prev_week_mort_rate, na.rm = TRUE)
cat("SD of log incidence:", round(sd_log_inc, 3), "\n")
cat("SD of log mortality:", round(sd_log_mort, 3), "\n")

# --- Contacts response to INCIDENCE by party ---
# County FE: within-county variation over time
# No wave FE: to capture temporal variation in incidence
contacts_inc_model <- lm(num_cc ~ political_party * log_inc_std +
                           interview_day_weekend + city + COUNTY_FIPS,
                         data = bics_valid,
                         weights = weight_party_raked)

contacts_inc_tidy <- tidy(contacts_inc_model)

# Democrat is the reference category (alphabetically first)
# Main effect = Democrat slope (effect of 1 SD increase)
# Interactions = difference from Democrat
inc_main_effect <- contacts_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(estimate)

inc_ind_interaction <- contacts_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(estimate)

inc_rep_interaction <- contacts_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(estimate)

# Get p-values
inc_rep_int_p <- contacts_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(p.value)

inc_ind_int_p <- contacts_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(p.value)

# Effects are now directly the coefficients (1 SD increase)
# Democrat: main effect (reference category)
# Independent: main + ind_interaction
# Republican: main + rep_interaction
contacts_inc_effect_dem <- inc_main_effect
contacts_inc_effect_ind <- inc_main_effect + inc_ind_interaction
contacts_inc_effect_rep <- inc_main_effect + inc_rep_interaction

# Get SEs from model summary (raw SEs for standardized coefficients)
contacts_inc_se_dem <- contacts_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(std.error)
contacts_inc_se_rep <- contacts_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(std.error)
contacts_inc_se_ind <- contacts_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(std.error)

# --- Contacts response to MORTALITY by party ---
contacts_mort_model <- lm(num_cc ~ political_party * log_mort_std +
                            interview_day_weekend + city + COUNTY_FIPS,
                          data = bics_valid,
                          weights = weight_party_raked)

contacts_mort_tidy <- tidy(contacts_mort_model)

# Democrat is the reference category
# Main effect = Democrat slope (effect of 1 SD increase)
# Interactions = difference from Democrat
mort_main_effect <- contacts_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(estimate)

mort_ind_interaction <- contacts_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(estimate)

mort_rep_interaction <- contacts_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(estimate)

# Get p-values
mort_rep_int_p <- contacts_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(p.value)

mort_ind_int_p <- contacts_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(p.value)

# Effects are now directly the coefficients (1 SD increase)
contacts_mort_effect_dem <- mort_main_effect
contacts_mort_effect_ind <- mort_main_effect + mort_ind_interaction
contacts_mort_effect_rep <- mort_main_effect + mort_rep_interaction

# Get SEs (raw SEs for standardized coefficients)
contacts_mort_se_dem <- contacts_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(std.error)
contacts_mort_se_rep <- contacts_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(std.error)
contacts_mort_se_ind <- contacts_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(std.error)

# --- Masks response to INCIDENCE by party ---
masks_inc_model <- lm(Norm_Masks_Used ~ political_party * log_inc_std +
                        interview_day_weekend + city + COUNTY_FIPS,
                      data = bics_valid,
                      weights = weight_party_raked)

masks_inc_tidy <- tidy(masks_inc_model)

# Democrat is reference category for masks too
# Main effect = Democrat slope (effect of 1 SD increase)
masks_inc_main <- masks_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(estimate)

masks_inc_ind_int <- masks_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(estimate)

masks_inc_rep_int <- masks_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(estimate)

# Effects are now directly the coefficients (1 SD increase)
masks_inc_effect_dem <- masks_inc_main
masks_inc_effect_ind <- masks_inc_main + masks_inc_ind_int
masks_inc_effect_rep <- masks_inc_main + masks_inc_rep_int

# Get SEs for masks ~ incidence (raw SEs)
masks_inc_se_dem <- masks_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(std.error)
masks_inc_se_rep <- masks_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(std.error)
masks_inc_se_ind <- masks_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(std.error)

# --- Masks response to MORTALITY by party ---
masks_mort_model <- lm(Norm_Masks_Used ~ political_party * log_mort_std +
                         interview_day_weekend + city + COUNTY_FIPS,
                       data = bics_valid,
                       weights = weight_party_raked)

masks_mort_tidy <- tidy(masks_mort_model)

# Democrat is reference category (effect of 1 SD increase)
masks_mort_main <- masks_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(estimate)

masks_mort_ind_int <- masks_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(estimate)

masks_mort_rep_int <- masks_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(estimate)

# Effects are now directly the coefficients (1 SD increase)
masks_mort_effect_dem <- masks_mort_main
masks_mort_effect_ind <- masks_mort_main + masks_mort_ind_int
masks_mort_effect_rep <- masks_mort_main + masks_mort_rep_int

# Get SEs for masks ~ mortality (raw SEs)
masks_mort_se_dem <- masks_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(std.error)
masks_mort_se_rep <- masks_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(std.error)
masks_mort_se_ind <- masks_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(std.error)

# =============================================================================
# 6a2. PERCEPTION (CONCERN) RESPONSE TO PANDEMIC CONDITIONS
# =============================================================================

cat("Calculating perception response models...\n")

# --- Perception response to INCIDENCE by party ---
# Using linear probability model for interpretability (concern is 0/1)
# Multiply by 100 to get percentage point effects
concern_inc_model <- lm(binary_concern_strong ~ political_party * log_inc_std +
                          interview_day_weekend + city + COUNTY_FIPS,
                        data = bics_valid,
                        weights = weight_party_raked)

concern_inc_tidy <- tidy(concern_inc_model)

# Democrat is reference category
concern_inc_main <- concern_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(estimate)

concern_inc_ind_int <- concern_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(estimate)

concern_inc_rep_int <- concern_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(estimate)

# Effects in percentage points (multiply by 100)
concern_inc_effect_dem <- concern_inc_main * 100
concern_inc_effect_ind <- (concern_inc_main + concern_inc_ind_int) * 100
concern_inc_effect_rep <- (concern_inc_main + concern_inc_rep_int) * 100

# Get SEs (also convert to pp)
concern_inc_se_dem <- concern_inc_tidy %>%
  filter(term == "log_inc_std") %>%
  pull(std.error) * 100
concern_inc_se_ind <- concern_inc_tidy %>%
  filter(term == "political_partyIndependent:log_inc_std") %>%
  pull(std.error) * 100
concern_inc_se_rep <- concern_inc_tidy %>%
  filter(term == "political_partyRepublican:log_inc_std") %>%
  pull(std.error) * 100

# --- Perception response to MORTALITY by party ---
concern_mort_model <- lm(binary_concern_strong ~ political_party * log_mort_std +
                           interview_day_weekend + city + COUNTY_FIPS,
                         data = bics_valid,
                         weights = weight_party_raked)

concern_mort_tidy <- tidy(concern_mort_model)

# Democrat is reference category
concern_mort_main <- concern_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(estimate)

concern_mort_ind_int <- concern_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(estimate)

concern_mort_rep_int <- concern_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(estimate)

# Effects in percentage points
concern_mort_effect_dem <- concern_mort_main * 100
concern_mort_effect_ind <- (concern_mort_main + concern_mort_ind_int) * 100
concern_mort_effect_rep <- (concern_mort_main + concern_mort_rep_int) * 100

# Get SEs (also convert to pp)
concern_mort_se_dem <- concern_mort_tidy %>%
  filter(term == "log_mort_std") %>%
  pull(std.error) * 100
concern_mort_se_ind <- concern_mort_tidy %>%
  filter(term == "political_partyIndependent:log_mort_std") %>%
  pull(std.error) * 100
concern_mort_se_rep <- concern_mort_tidy %>%
  filter(term == "political_partyRepublican:log_mort_std") %>%
  pull(std.error) * 100

# =============================================================================
# 6b. VACCINATION RESPONSE TO PANDEMIC CONDITIONS (Wave 6 only)
# =============================================================================

cat("Calculating vaccination response models...\n")

# Filter to wave 6 for vaccination analysis
bics_wave6 <- bics_valid %>% filter(wave == 6)

# Calculate mean log mortality for wave 6 specifically
mean_log_mort_w6 <- mean(bics_wave6$log_prev_week_mort_rate, na.rm = TRUE)
mort_delta_w6 <- mean_log_mort_w6 * 0.10

mean_log_inc_w6 <- mean(bics_wave6$log_prev_week_inc_rate, na.rm = TRUE)
inc_delta_w6 <- mean_log_inc_w6 * 0.10

# --- Vaccine acceptance response to MORTALITY by party ---
# Note: Vaccine acceptance is wave 6 only (cross-sectional), so we use STATE FE
# rather than county FE (which would leave no within-county variation)
vax_mort_model <- lm(vax_acceptance ~ political_party * log_prev_week_mort_rate +
                       interview_day_weekend + city + STATE,
                     data = bics_wave6,
                     weights = weight_party_raked)

vax_mort_tidy <- tidy(vax_mort_model)

# Democrat is reference category
vax_mort_main <- vax_mort_tidy %>%
  filter(term == "log_prev_week_mort_rate") %>%
  pull(estimate)

vax_mort_ind_int <- vax_mort_tidy %>%
  filter(term == "political_partyIndependent:log_prev_week_mort_rate") %>%
  pull(estimate)

vax_mort_rep_int <- vax_mort_tidy %>%
  filter(term == "political_partyRepublican:log_prev_week_mort_rate") %>%
  pull(estimate)

# Use 0 if interaction terms don't exist (model may not have converged)
if (length(vax_mort_ind_int) == 0) vax_mort_ind_int <- 0
if (length(vax_mort_rep_int) == 0) vax_mort_rep_int <- 0

# Vaccination effects (convert to percentage points by multiplying by 100)
vax_mort_effect_dem <- vax_mort_main * mort_delta_w6 * 100
vax_mort_effect_ind <- (vax_mort_main + vax_mort_ind_int) * mort_delta_w6 * 100
vax_mort_effect_rep <- (vax_mort_main + vax_mort_rep_int) * mort_delta_w6 * 100

# --- Vaccine acceptance response to INCIDENCE by party ---
# Note: Vaccine acceptance is wave 6 only (cross-sectional), so we use STATE FE
vax_inc_model <- lm(vax_acceptance ~ political_party * log_prev_week_inc_rate +
                      interview_day_weekend + city + STATE,
                    data = bics_wave6,
                    weights = weight_party_raked)

vax_inc_tidy <- tidy(vax_inc_model)

# Democrat is reference category
vax_inc_main <- vax_inc_tidy %>%
  filter(term == "log_prev_week_inc_rate") %>%
  pull(estimate)

vax_inc_ind_int <- vax_inc_tidy %>%
  filter(term == "political_partyIndependent:log_prev_week_inc_rate") %>%
  pull(estimate)

vax_inc_rep_int <- vax_inc_tidy %>%
  filter(term == "political_partyRepublican:log_prev_week_inc_rate") %>%
  pull(estimate)

# Use 0 if interaction terms don't exist
if (length(vax_inc_ind_int) == 0) vax_inc_ind_int <- 0
if (length(vax_inc_rep_int) == 0) vax_inc_rep_int <- 0

# Vaccination effects (convert to percentage points)
vax_inc_effect_dem <- vax_inc_main * inc_delta_w6 * 100
vax_inc_effect_ind <- (vax_inc_main + vax_inc_ind_int) * inc_delta_w6 * 100
vax_inc_effect_rep <- (vax_inc_main + vax_inc_rep_int) * inc_delta_w6 * 100

# =============================================================================
# 6c. WAVE-BY-WAVE INCIDENCE RESPONSIVENESS (for appendix)
# =============================================================================

cat("Calculating wave-by-wave incidence responsiveness...\n")

# Run separate models for each wave to show geographic variation in responsiveness
# Within each wave, variation in incidence is primarily geographic (cross-sectional)

wave_responsiveness <- map_df(1:6, function(w) {
  wave_data <- bics_valid %>% filter(wave == w)

  # Skip if insufficient data
  if (nrow(wave_data) < 100) return(NULL)

  # Calculate wave-specific delta
  mean_log_inc_wave <- mean(wave_data$log_prev_week_inc_rate, na.rm = TRUE)
  delta_wave <- mean_log_inc_wave * 0.10

  # Contacts ~ incidence model for this wave
  contacts_model <- tryCatch({
    lm(num_cc ~ political_party * log_prev_week_inc_rate +
         interview_day_weekend + city + STATE,
       data = wave_data,
       weights = weight_party_raked)
  }, error = function(e) NULL)

  if (is.null(contacts_model)) return(NULL)

  contacts_tidy <- tidy(contacts_model)

  # Extract coefficients (Democrat is reference)
  main_effect <- contacts_tidy %>%
    filter(term == "log_prev_week_inc_rate") %>%
    pull(estimate)

  ind_int <- contacts_tidy %>%
    filter(term == "political_partyIndependent:log_prev_week_inc_rate") %>%
    pull(estimate)

  rep_int <- contacts_tidy %>%
    filter(term == "political_partyRepublican:log_prev_week_inc_rate") %>%
    pull(estimate)

  # Handle missing interactions
  if (length(main_effect) == 0) main_effect <- NA
  if (length(ind_int) == 0) ind_int <- 0
  if (length(rep_int) == 0) rep_int <- 0

  # Get standard errors
  main_se <- contacts_tidy %>%
    filter(term == "log_prev_week_inc_rate") %>%
    pull(std.error)

  rep_int_se <- contacts_tidy %>%
    filter(term == "political_partyRepublican:log_prev_week_inc_rate") %>%
    pull(std.error)

  ind_int_se <- contacts_tidy %>%
    filter(term == "political_partyIndependent:log_prev_week_inc_rate") %>%
    pull(std.error)

  if (length(main_se) == 0) main_se <- NA
  if (length(rep_int_se) == 0) rep_int_se <- NA
  if (length(ind_int_se) == 0) ind_int_se <- NA

  # Calculate effects
  tibble(
    wave = w,
    n = nrow(wave_data),
    mean_inc_rate = mean(wave_data$prev_week_inc_rate, na.rm = TRUE),
    effect_dem = main_effect * delta_wave,
    effect_ind = (main_effect + ind_int) * delta_wave,
    effect_rep = (main_effect + rep_int) * delta_wave,
    se_dem = main_se * delta_wave,
    se_ind = ind_int_se * delta_wave,
    se_rep = rep_int_se * delta_wave,
    gap_change = (main_effect + rep_int) * delta_wave - main_effect * delta_wave  # Rep - Dem
  )
})

# Save wave-by-wave results
wave_results_path <- here("out", "tables", "wave_incidence_responsiveness.csv")
write_csv(wave_responsiveness, wave_results_path)

cat("Wave-by-wave results saved to:", wave_results_path, "\n")

# =============================================================================
# 7. COMPILE ALL STATISTICS
# =============================================================================

cat("Compiling statistics...\n")

# Helper to extract values
get_val <- function(df, party, var) {
  df %>% filter(political_party == party) %>% pull(!!sym(var))
}

paper_stats <- list(
  # Sample sizes
  n_total = n_total,
  n_dem = n_dem,
  n_rep = n_rep,
  n_ind = n_ind,
  pct_dem = n_dem / n_total * 100,
  pct_rep = n_rep / n_total * 100,
  pct_ind = n_ind / n_total * 100,

  # Demographics
  age_dem = get_val(demo_by_party, "Democrat", "avg_age"),
  age_rep = get_val(demo_by_party, "Republican", "avg_age"),
  age_ind = get_val(demo_by_party, "Independent", "avg_age"),

  male_dem = get_val(demo_by_party, "Democrat", "pct_male"),
  male_rep = get_val(demo_by_party, "Republican", "pct_male"),
  male_ind = get_val(demo_by_party, "Independent", "pct_male"),

  white_dem = get_val(demo_by_party, "Democrat", "pct_white"),
  white_rep = get_val(demo_by_party, "Republican", "pct_white"),
  white_ind = get_val(demo_by_party, "Independent", "pct_white"),

  metro_dem = get_val(demo_by_party, "Democrat", "pct_metro"),
  metro_rep = get_val(demo_by_party, "Republican", "pct_metro"),
  metro_ind = get_val(demo_by_party, "Independent", "pct_metro"),

  college_dem = get_val(demo_by_party, "Democrat", "pct_college"),
  college_rep = get_val(demo_by_party, "Republican", "pct_college"),
  college_ind = get_val(demo_by_party, "Independent", "pct_college"),

  employed_dem = get_val(demo_by_party, "Democrat", "pct_employed"),
  employed_rep = get_val(demo_by_party, "Republican", "pct_employed"),
  employed_ind = get_val(demo_by_party, "Independent", "pct_employed"),

  hhsize_avg = weighted.mean(bics_valid$resp_hhsize, w = bics_valid$weight_party_raked, na.rm = TRUE),
  hhsize_dem = get_val(demo_by_party, "Democrat", "avg_hhsize"),
  hhsize_rep = get_val(demo_by_party, "Republican", "avg_hhsize"),
  hhsize_ind = get_val(demo_by_party, "Independent", "avg_hhsize"),

  # Health behaviors
  contacts_dem = get_val(behaviors_by_party, "Democrat", "total_contacts"),
  contacts_rep = get_val(behaviors_by_party, "Republican", "total_contacts"),
  contacts_ind = get_val(behaviors_by_party, "Independent", "total_contacts"),
  contacts_gap = get_val(behaviors_by_party, "Republican", "total_contacts") -
                 get_val(behaviors_by_party, "Democrat", "total_contacts"),

  nonhh_dem = get_val(behaviors_by_party, "Democrat", "nonhh_contacts"),
  nonhh_rep = get_val(behaviors_by_party, "Republican", "nonhh_contacts"),
  nonhh_ind = get_val(behaviors_by_party, "Independent", "nonhh_contacts"),

  mask_dem = get_val(behaviors_by_party, "Democrat", "mask_usage"),
  mask_rep = get_val(behaviors_by_party, "Republican", "mask_usage"),
  mask_ind = get_val(behaviors_by_party, "Independent", "mask_usage"),
  mask_gap = get_val(behaviors_by_party, "Democrat", "mask_usage") -
             get_val(behaviors_by_party, "Republican", "mask_usage"),

  # Pandemic perceptions
  concern_dem = concern_dem,
  concern_rep = concern_rep,
  concern_ind = concern_ind,
  concern_gap = concern_gap,
  any_concern_dem = any_concern_dem,
  any_concern_rep = any_concern_rep,
  any_concern_ind = any_concern_ind,

  # Behaviors among "very concerned" respondents
  contacts_vc_dem = contacts_vc_dem,
  contacts_vc_rep = contacts_vc_rep,
  contacts_vc_ind = contacts_vc_ind,
  contacts_vc_gap = contacts_vc_gap,
  nonhh_vc_dem = nonhh_vc_dem,
  nonhh_vc_rep = nonhh_vc_rep,
  nonhh_vc_ind = nonhh_vc_ind,
  nonhh_vc_gap = nonhh_vc_gap,
  mask_vc_dem = mask_vc_dem,
  mask_vc_rep = mask_vc_rep,
  mask_vc_ind = mask_vc_ind,
  mask_vc_gap = mask_vc_gap,
  n_vc_dem = n_vc_dem,
  n_vc_rep = n_vc_rep,
  n_vc_ind = n_vc_ind,

  # Mask usage by concern level (for slope analysis)
  mask_notconcerned_dem = mask_notconcerned_dem,
  mask_notconcerned_rep = mask_notconcerned_rep,
  mask_notvery_dem = mask_notvery_dem,
  mask_notvery_rep = mask_notvery_rep,
  mask_somewhat_dem = mask_somewhat_dem,
  mask_somewhat_rep = mask_somewhat_rep,
  mask_veryconcerned_dem = mask_veryconcerned_dem,
  mask_veryconcerned_rep = mask_veryconcerned_rep,
  mask_slope_dem = mask_slope_dem,
  mask_slope_rep = mask_slope_rep,

  # Interaction model: concern and mortality as moderators of partisan gap
  contact_int_baseline = contact_int_baseline,
  contact_int_concern = contact_int_concern,
  contact_int_mort = contact_int_mort,
  contact_int_concern_se = contact_int_concern_se,
  contact_int_mort_se = contact_int_mort_se,
  contact_int_concern_p = contact_int_concern_p,
  contact_int_mort_p = contact_int_mort_p,

  mask_int_baseline = mask_int_baseline,
  mask_int_concern = mask_int_concern,
  mask_int_mort = mask_int_mort,
  mask_int_concern_se = mask_int_concern_se,
  mask_int_mort_se = mask_int_mort_se,
  mask_int_concern_p = mask_int_concern_p,
  mask_int_mort_p = mask_int_mort_p,

  # Interaction model with COUNTY FE (local variation)
  contact_county_baseline = contact_county_baseline,
  contact_county_concern = contact_county_concern,
  contact_county_mort = contact_county_mort,
  contact_county_concern_se = contact_county_concern_se,
  contact_county_mort_se = contact_county_mort_se,
  contact_county_concern_p = contact_county_concern_p,
  contact_county_mort_p = contact_county_mort_p,

  mask_county_baseline = mask_county_baseline,
  mask_county_concern = mask_county_concern,
  mask_county_mort = mask_county_mort,
  mask_county_concern_se = mask_county_concern_se,
  mask_county_mort_se = mask_county_mort_se,
  mask_county_concern_p = mask_county_concern_p,
  mask_county_mort_p = mask_county_mort_p,

  # Vaccination
  vax_dem = get_val(vax_by_party, "Democrat", "pct_vaccinated"),
  vax_rep = get_val(vax_by_party, "Republican", "pct_vaccinated"),
  vax_ind = get_val(vax_by_party, "Independent", "pct_vaccinated"),
  vax_gap = get_val(vax_by_party, "Democrat", "pct_vaccinated") -
            get_val(vax_by_party, "Republican", "pct_vaccinated"),

  # Vaccination intentions
  will_vax_dem = get_val(vax_intent_by_party, "Democrat", "pct_will_vax"),
  will_vax_rep = get_val(vax_intent_by_party, "Republican", "pct_will_vax"),
  wont_vax_dem = get_val(vax_intent_by_party, "Democrat", "pct_wont_vax"),
  wont_vax_rep = get_val(vax_intent_by_party, "Republican", "pct_wont_vax"),

  # Pandemic response: contacts ~ incidence (1 SD increase, standardized)
  contacts_inc_effect_dem = contacts_inc_effect_dem,
  contacts_inc_effect_rep = contacts_inc_effect_rep,
  contacts_inc_effect_ind = contacts_inc_effect_ind,
  contacts_inc_se_dem = contacts_inc_se_dem,
  contacts_inc_se_rep = contacts_inc_se_rep,
  contacts_inc_se_ind = contacts_inc_se_ind,

  # Pandemic response: contacts ~ mortality (1 SD increase, standardized)
  contacts_mort_effect_dem = contacts_mort_effect_dem,
  contacts_mort_effect_rep = contacts_mort_effect_rep,
  contacts_mort_effect_ind = contacts_mort_effect_ind,
  contacts_mort_se_dem = contacts_mort_se_dem,
  contacts_mort_se_rep = contacts_mort_se_rep,
  contacts_mort_se_ind = contacts_mort_se_ind,

  # Interaction coefficients and p-values (contacts ~ incidence)
  # Democrat is reference; these are differences from Democrat
  inc_main_effect = inc_main_effect,
  inc_ind_interaction = inc_ind_interaction,
  inc_rep_interaction = inc_rep_interaction,
  inc_ind_int_p = inc_ind_int_p,
  inc_rep_int_p = inc_rep_int_p,

  # Interaction coefficients and p-values (contacts ~ mortality)
  # Democrat is reference; these are differences from Democrat
  mort_main_effect = mort_main_effect,
  mort_ind_interaction = mort_ind_interaction,
  mort_rep_interaction = mort_rep_interaction,
  mort_ind_int_p = mort_ind_int_p,
  mort_rep_int_p = mort_rep_int_p,

  # Pandemic response: masks ~ incidence (1 SD increase, standardized)
  masks_inc_effect_dem = masks_inc_effect_dem,
  masks_inc_effect_rep = masks_inc_effect_rep,
  masks_inc_effect_ind = masks_inc_effect_ind,
  masks_inc_se_dem = masks_inc_se_dem,
  masks_inc_se_rep = masks_inc_se_rep,
  masks_inc_se_ind = masks_inc_se_ind,

  # Pandemic response: masks ~ mortality (1 SD increase, standardized)
  masks_mort_effect_dem = masks_mort_effect_dem,
  masks_mort_effect_rep = masks_mort_effect_rep,
  masks_mort_effect_ind = masks_mort_effect_ind,
  masks_mort_se_dem = masks_mort_se_dem,
  masks_mort_se_rep = masks_mort_se_rep,
  masks_mort_se_ind = masks_mort_se_ind,

  # Pandemic response: concern ~ incidence (1 SD increase, pp)
  concern_inc_effect_dem = concern_inc_effect_dem,
  concern_inc_effect_rep = concern_inc_effect_rep,
  concern_inc_effect_ind = concern_inc_effect_ind,
  concern_inc_se_dem = concern_inc_se_dem,
  concern_inc_se_rep = concern_inc_se_rep,
  concern_inc_se_ind = concern_inc_se_ind,

  # Pandemic response: concern ~ mortality (1 SD increase, pp)
  concern_mort_effect_dem = concern_mort_effect_dem,
  concern_mort_effect_rep = concern_mort_effect_rep,
  concern_mort_effect_ind = concern_mort_effect_ind,
  concern_mort_se_dem = concern_mort_se_dem,
  concern_mort_se_rep = concern_mort_se_rep,
  concern_mort_se_ind = concern_mort_se_ind,

  # Pandemic response: vaccination ~ incidence (10% increase effects, pp)
  vax_inc_effect_dem = vax_inc_effect_dem,
  vax_inc_effect_rep = vax_inc_effect_rep,
  vax_inc_effect_ind = vax_inc_effect_ind,

  # Pandemic response: vaccination ~ mortality (10% increase effects, pp)
  vax_mort_effect_dem = vax_mort_effect_dem,
  vax_mort_effect_rep = vax_mort_effect_rep,
  vax_mort_effect_ind = vax_mort_effect_ind
)

# =============================================================================
# 6b. ADJUSTED BEHAVIORS BY PARTY AND WAVE (EMMEANS)
# =============================================================================

cat("Calculating adjusted behaviors by party and wave (emmeans)...\n")

library(emmeans)

# Prepare data for models - need complete cases for covariates in simplified model
# Removed STATE (51 levels), r_working, r_metro to reduce model complexity and missing data
model_data <- bics_valid %>%
  filter(
    !is.na(num_cc) & !is.na(Norm_Masks_Used) &
    !is.na(political_party) & !is.na(wave) &
    !is.na(interview_day_weekend) & !is.na(city) &
    !is.na(age_group) & !is.na(r_race) & !is.na(resp_hispanic) &
    !is.na(r_male) & !is.na(resp_educ) & !is.na(resp_hhsize) &
    !is.na(CD_PERCENT_DEMOCRAT) & !is.na(log_prev_week_mort_rate)
  ) %>%
  mutate(
    wave = factor(wave),
    political_party = factor(political_party, levels = c("Republican", "Independent", "Democrat"))
  )

# Check if county_mask_mandate exists
has_mask_mandate <- "county_mask_mandate" %in% names(model_data) &&
                    sum(!is.na(model_data$county_mask_mandate)) > 100

# Calculate average values for emmeans predictions
avg_hhsize <- weighted.mean(model_data$resp_hhsize, w = model_data$weight_party_raked, na.rm = TRUE)
avg_mort <- weighted.mean(model_data$log_prev_week_mort_rate, w = model_data$weight_party_raked, na.rm = TRUE)
avg_CD_dem <- weighted.mean(model_data$CD_PERCENT_DEMOCRAT, w = model_data$weight_party_raked, na.rm = TRUE)

# Model with party*wave interaction for CONTACTS
# Simplified model: removed STATE (51 levels, captured by CD_PERCENT_DEMOCRAT)
# and r_working/r_metro (many missing, limited confounding)
contacts_wave_model <- lm(
  num_cc ~ political_party * wave + interview_day_weekend + city +
    age_group + r_race + resp_hispanic + r_male + resp_educ +
    resp_hhsize + CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data,
  weights = weight_party_raked
)

# Extract emmeans for contacts by party and wave
# Use nuisance argument to average over factors that don't interact with party/wave
contacts_emm <- emmeans(contacts_wave_model,
                        specs = ~ political_party * wave,
                        nuisance = c("interview_day_weekend", "city", "age_group",
                                     "r_race", "resp_hispanic", "r_male", "resp_educ"))

contacts_emm_df <- as.data.frame(summary(contacts_emm)) %>%
  rename(estimate = emmean, std_error = SE) %>%
  mutate(outcome = "contacts")

# Model with party*wave interaction for MASKS
# Simplified model: same controls as contacts
masks_wave_model <- lm(
  Norm_Masks_Used ~ political_party * wave + interview_day_weekend + city +
    age_group + r_race + resp_hispanic + r_male + resp_educ +
    resp_hhsize + CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data,
  weights = weight_party_raked
)

# Extract emmeans for masks by party and wave
# Use nuisance argument to average over factors that don't interact with party/wave
masks_emm <- emmeans(masks_wave_model,
                     specs = ~ political_party * wave,
                     nuisance = c("interview_day_weekend", "city", "age_group",
                                  "r_race", "resp_hispanic", "r_male", "resp_educ"))

masks_emm_df <- as.data.frame(summary(masks_emm)) %>%
  rename(estimate = emmean, std_error = SE) %>%
  mutate(outcome = "masks")

# Model with party*wave interaction for PERCEPTION (binary_concern_strong)
# Filter to those with non-missing concern
model_data_concern <- model_data %>%
  filter(!is.na(binary_concern_strong))

concern_wave_model <- lm(
  binary_concern_strong ~ political_party * wave + interview_day_weekend + city +
    age_group + r_race + resp_hispanic + r_male + resp_educ +
    resp_hhsize + CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data_concern,
  weights = weight_party_raked
)

# Extract emmeans for concern by party and wave
concern_emm <- emmeans(concern_wave_model,
                       specs = ~ political_party * wave,
                       nuisance = c("interview_day_weekend", "city", "age_group",
                                    "r_race", "resp_hispanic", "r_male", "resp_educ"))

concern_emm_df <- as.data.frame(summary(concern_emm)) %>%
  rename(estimate = emmean, std_error = SE) %>%
  mutate(
    outcome = "concern",
    # Convert to percentage scale for consistency
    estimate = estimate * 100,
    std_error = std_error * 100,
    lower.CL = lower.CL * 100,
    upper.CL = upper.CL * 100
  )

# Combine emmeans results
adjusted_behaviors_by_wave <- bind_rows(contacts_emm_df, masks_emm_df, concern_emm_df) %>%
  select(outcome, wave, political_party, estimate, std_error, lower.CL, upper.CL)

# Calculate adjusted gaps by wave
adjusted_gaps_by_wave <- adjusted_behaviors_by_wave %>%
  select(outcome, wave, political_party, estimate) %>%
  pivot_wider(names_from = political_party, values_from = estimate) %>%
  mutate(
    gap_rep_dem = Republican - Democrat,
    gap_ind_dem = Independent - Democrat
  )

# Export adjusted behaviors
write_csv(adjusted_behaviors_by_wave, here("out", "tables", "adjusted_behaviors_by_wave.csv"))
write_csv(adjusted_gaps_by_wave, here("out", "tables", "adjusted_gaps_by_wave.csv"))

cat("  Adjusted behaviors exported to adjusted_behaviors_by_wave.csv\n")

# =============================================================================
# 6c. BEHAVIORS BY PARTY AND POLICY ENVIRONMENT
# =============================================================================

cat("Calculating behaviors by party and policy environment...\n")

# Filter to clear policy categories (exclude unclear/unspecified)
policy_data <- bics_valid %>%
  filter(county_mask_mandate %in% c("Strict", "Less Strict", "No Mandate")) %>%
  mutate(
    policy = factor(county_mask_mandate,
                    levels = c("No Mandate", "Less Strict", "Strict")),
    political_party = factor(political_party,
                             levels = c("Republican", "Independent", "Democrat"))
  )

# Calculate mean behaviors by party and policy
behaviors_by_policy <- policy_data %>%
  group_by(policy, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_pooled, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_pooled^2 * (num_cc - weighted.mean(num_cc, w = weight_pooled, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_pooled, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_pooled, na.rm = TRUE),
    mask_se = sqrt(sum(weight_pooled^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_pooled, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_pooled, na.rm = TRUE),
    .groups = "drop"
  )

# Export
write_csv(behaviors_by_policy, here("out", "tables", "behaviors_by_policy.csv"))
cat("  Behaviors by policy exported to behaviors_by_policy.csv\n")

# =============================================================================
# 6e. BEHAVIORS BY CENSUS REGION AND PARTY
# =============================================================================

cat("Calculating behaviors by census region and party...\n")

# Calculate behaviors by region and party
behaviors_by_region <- bics_valid %>%
  filter(!is.na(census_region)) %>%
  group_by(census_region, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_party_raked^2 * (num_cc - weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    mask_se = sqrt(sum(weight_party_raked^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate partisan gaps by region
gaps_by_region <- behaviors_by_region %>%
  select(census_region, political_party, contacts_mean, mask_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_mean, mask_mean)
  ) %>%
  mutate(
    contacts_gap_rd = contacts_mean_Republican - contacts_mean_Democrat,
    contacts_gap_id = contacts_mean_Independent - contacts_mean_Democrat,
    mask_gap_dr = mask_mean_Democrat - mask_mean_Republican,
    mask_gap_di = mask_mean_Democrat - mask_mean_Independent
  )

# Calculate vaccine acceptance by region (wave 6 only)
vax_by_region <- bics_valid %>%
  filter(wave == 6, !is.na(census_region), !is.na(vax_acceptance)) %>%
  group_by(census_region, political_party) %>%
  summarise(
    n = n(),
    vax_mean = weighted.mean(vax_acceptance, w = weight_party_raked, na.rm = TRUE) * 100,
    vax_se = sqrt(sum(weight_party_raked^2 * (vax_acceptance - weighted.mean(vax_acceptance, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
             sum(weight_party_raked, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Calculate perception (concern) by region
perception_by_region <- bics_valid %>%
  filter(!is.na(census_region), !is.na(binary_concern_strong)) %>%
  group_by(census_region, political_party) %>%
  summarise(
    n = n(),
    concern_mean = weighted.mean(binary_concern_strong, w = weight_party_raked, na.rm = TRUE) * 100,
    concern_se = sqrt(sum(weight_party_raked^2 * (binary_concern_strong - weighted.mean(binary_concern_strong, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                 sum(weight_party_raked, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Export
write_csv(behaviors_by_region, here("out", "tables", "behaviors_by_region.csv"))
write_csv(gaps_by_region, here("out", "tables", "gaps_by_region.csv"))
write_csv(vax_by_region, here("out", "tables", "vax_by_region.csv"))
write_csv(perception_by_region, here("out", "tables", "perception_by_region.csv"))
cat("  Behaviors by region exported to behaviors_by_region.csv, gaps_by_region.csv, vax_by_region.csv, perception_by_region.csv\n")

# =============================================================================
# 6f. BEHAVIORS BY PARTY AND METRO STATUS
# =============================================================================

cat("Calculating behaviors by party and metro status...\n")

# Calculate contacts and masks by party and metro (all waves)
behaviors_by_metro <- bics_valid %>%
  filter(!is.na(Metro)) %>%
  mutate(metro_status = ifelse(Metro == 1, "Metropolitan", "Non-Metropolitan")) %>%
  group_by(metro_status, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_party_raked^2 * (num_cc - weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    mask_se = sqrt(sum(weight_party_raked^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate vaccine acceptance by party and metro (wave 6 only)
vax_by_metro <- bics_valid %>%
  filter(wave == 6, !is.na(Metro), !is.na(vax_acceptance)) %>%
  mutate(metro_status = ifelse(Metro == 1, "Metropolitan", "Non-Metropolitan")) %>%
  group_by(metro_status, political_party) %>%
  summarise(
    n = n(),
    vax_mean = weighted.mean(vax_acceptance, w = weight_party_raked, na.rm = TRUE) * 100,
    vax_se = sqrt(sum(weight_party_raked^2 * (vax_acceptance - weighted.mean(vax_acceptance, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
             sum(weight_party_raked, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Merge into single data frame
behaviors_by_metro_full <- behaviors_by_metro %>%
  left_join(vax_by_metro %>% select(metro_status, political_party, vax_mean, vax_se),
            by = c("metro_status", "political_party"))

write_csv(behaviors_by_metro_full, here("out", "tables", "behaviors_by_metro.csv"))
cat("  Behaviors by metro exported to behaviors_by_metro.csv\n")

# --- ADJUSTED METRO ESTIMATES (EMMEANS WITH CONTROLS) ---

cat("Calculating adjusted metro estimates (emmeans with wave and mortality controls)...\n")

# Prepare data
bics_metro <- bics_valid %>%
  filter(!is.na(Metro), !is.na(prev_week_mort_rate)) %>%
  mutate(
    metro_status = factor(ifelse(Metro == 1, "Metropolitan", "Non-Metropolitan")),
    log_mort = log(prev_week_mort_rate + 0.001),
    wave_factor = factor(wave)
  )

# Contacts model
contacts_metro_model <- lm(num_cc ~ political_party * metro_status + wave_factor + log_mort,
                           data = bics_metro, weights = weight_party_raked)

# Mask model
mask_metro_model <- lm(Norm_Masks_Used ~ political_party * metro_status + wave_factor + log_mort,
                       data = bics_metro, weights = weight_party_raked)

# Vaccine acceptance model (wave 6 only)
bics_metro_vax <- bics_valid %>%
  filter(wave == 6, !is.na(Metro), !is.na(vax_acceptance), !is.na(prev_week_mort_rate)) %>%
  mutate(
    metro_status = factor(ifelse(Metro == 1, "Metropolitan", "Non-Metropolitan")),
    log_mort = log(prev_week_mort_rate + 0.001)
  )

vax_metro_model <- lm(vax_acceptance ~ political_party * metro_status + log_mort,
                      data = bics_metro_vax, weights = weight_party_raked)

# Extract emmeans
contacts_metro_emmeans <- emmeans(contacts_metro_model, ~ political_party | metro_status)
mask_metro_emmeans <- emmeans(mask_metro_model, ~ political_party | metro_status)
vax_metro_emmeans <- emmeans(vax_metro_model, ~ political_party | metro_status)

# Combine into data frame
adj_behaviors_by_metro <- bind_rows(
  as.data.frame(contacts_metro_emmeans) %>%
    mutate(contacts_adj_mean = emmean, contacts_adj_se = SE) %>%
    select(metro_status, political_party, contacts_adj_mean, contacts_adj_se),
  .id = NULL
)

adj_masks_by_metro <- as.data.frame(mask_metro_emmeans) %>%
  mutate(mask_adj_mean = emmean, mask_adj_se = SE) %>%
  select(metro_status, political_party, mask_adj_mean, mask_adj_se)

adj_vax_by_metro <- as.data.frame(vax_metro_emmeans) %>%
  mutate(vax_adj_mean = emmean * 100, vax_adj_se = SE * 100) %>%
  select(metro_status, political_party, vax_adj_mean, vax_adj_se)

# Merge all
adj_metro_full <- as.data.frame(contacts_metro_emmeans) %>%
  mutate(contacts_adj_mean = emmean, contacts_adj_se = SE) %>%
  select(metro_status, political_party, contacts_adj_mean, contacts_adj_se) %>%
  left_join(adj_masks_by_metro, by = c("metro_status", "political_party")) %>%
  left_join(adj_vax_by_metro, by = c("metro_status", "political_party"))

write_csv(adj_metro_full, here("out", "tables", "adj_behaviors_by_metro.csv"))
cat("  Adjusted metro estimates exported to adj_behaviors_by_metro.csv\n")

# Calculate state-level behaviors and gaps (for mapping)
behaviors_by_state <- bics_valid %>%
  group_by(STATE, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate partisan gaps by state (need min 10 per party)
gaps_by_state <- behaviors_by_state %>%
  filter(n >= 10) %>%
  select(STATE, political_party, contacts_mean, mask_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_mean, mask_mean)
  ) %>%
  filter(!is.na(contacts_mean_Republican) & !is.na(contacts_mean_Democrat)) %>%
  mutate(
    contacts_gap_rd = contacts_mean_Republican - contacts_mean_Democrat,
    mask_gap_dr = mask_mean_Democrat - mask_mean_Republican
  )

write_csv(gaps_by_state, here("out", "tables", "gaps_by_state.csv"))
cat("  State-level gaps exported to gaps_by_state.csv\n")

# Calculate division-level behaviors and gaps
behaviors_by_division <- bics_valid %>%
  filter(!is.na(census_division)) %>%
  group_by(census_division, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_party_raked^2 * (num_cc - weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    mask_se = sqrt(sum(weight_party_raked^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

gaps_by_division <- behaviors_by_division %>%
  select(census_division, political_party, contacts_mean, mask_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_mean, mask_mean)
  ) %>%
  mutate(
    contacts_gap_rd = contacts_mean_Republican - contacts_mean_Democrat,
    mask_gap_dr = mask_mean_Democrat - mask_mean_Republican
  )

write_csv(behaviors_by_division, here("out", "tables", "behaviors_by_division.csv"))
write_csv(gaps_by_division, here("out", "tables", "gaps_by_division.csv"))
cat("  Division-level data exported to behaviors_by_division.csv, gaps_by_division.csv\n")

# =============================================================================
# 6f. ADJUSTED REGIONAL ESTIMATES (EMMEANS WITH CONTROLS)
# =============================================================================
# Control for wave and mortality rates to get cleaner regional estimates

cat("Calculating adjusted regional estimates (emmeans with wave and mortality controls)...\n")

library(emmeans)

# Prepare data with log mortality (handle zeros/NAs)
bics_region <- bics_valid %>%
  filter(!is.na(census_region), !is.na(prev_week_mort_rate)) %>%
  mutate(
    log_mort = log(prev_week_mort_rate + 0.001),
    wave_factor = factor(wave)
  )

# --- CENSUS REGION ADJUSTED ESTIMATES ---

# Contacts model with controls
contacts_region_model <- lm(num_cc ~ political_party * census_region +
                              wave_factor + log_mort,
                            data = bics_region,
                            weights = weight_party_raked)

# Mask model with controls
mask_region_model <- lm(Norm_Masks_Used ~ political_party * census_region +
                          wave_factor + log_mort,
                        data = bics_region,
                        weights = weight_party_raked)

# Perception model with controls
perception_region_model <- lm(binary_concern_strong ~ political_party * census_region +
                                wave_factor + log_mort,
                              data = bics_region %>% filter(!is.na(binary_concern_strong)),
                              weights = weight_party_raked)

# Vaccine acceptance model (wave 6 only, no wave control needed)
bics_region_vax <- bics_valid %>%
  filter(wave == 6, !is.na(census_region), !is.na(vax_acceptance), !is.na(prev_week_mort_rate)) %>%
  mutate(log_mort = log(prev_week_mort_rate + 0.001))

vax_region_model <- lm(vax_acceptance ~ political_party * census_region + log_mort,
                       data = bics_region_vax,
                       weights = weight_party_raked)

# Extract emmeans for region x party
contacts_region_emmeans <- emmeans(contacts_region_model, ~ political_party | census_region)
mask_region_emmeans <- emmeans(mask_region_model, ~ political_party | census_region)
perception_region_emmeans <- emmeans(perception_region_model, ~ political_party | census_region)
vax_region_emmeans <- emmeans(vax_region_model, ~ political_party | census_region)

# Convert to data frames
adj_behaviors_by_region <- bind_rows(
  as.data.frame(contacts_region_emmeans) %>%
    mutate(outcome = "contacts") %>%
    rename(estimate = emmean, adj_se = SE),
  as.data.frame(mask_region_emmeans) %>%
    mutate(outcome = "mask") %>%
    rename(estimate = emmean, adj_se = SE)
) %>%
  select(census_region, political_party, outcome, estimate, adj_se) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(estimate, adj_se),
    names_glue = "{outcome}_{.value}"
  ) %>%
  rename(
    contacts_adj_mean = contacts_estimate,
    contacts_adj_se = contacts_adj_se,
    mask_adj_mean = mask_estimate,
    mask_adj_se = mask_adj_se
  )

# Separate data frames for vaccination and perception (different scales)
adj_vax_by_region <- as.data.frame(vax_region_emmeans) %>%
  mutate(
    vax_adj_mean = emmean * 100,  # Convert to percentage
    vax_adj_se = SE * 100
  ) %>%
  select(census_region, political_party, vax_adj_mean, vax_adj_se)

adj_perception_by_region <- as.data.frame(perception_region_emmeans) %>%
  mutate(
    concern_adj_mean = emmean * 100,  # Convert to percentage
    concern_adj_se = SE * 100
  ) %>%
  select(census_region, political_party, concern_adj_mean, concern_adj_se)

# Calculate adjusted gaps by region
adj_gaps_by_region <- adj_behaviors_by_region %>%
  select(census_region, political_party, contacts_adj_mean, mask_adj_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_adj_mean, mask_adj_mean)
  ) %>%
  mutate(
    contacts_gap_rd = contacts_adj_mean_Republican - contacts_adj_mean_Democrat,
    contacts_gap_id = contacts_adj_mean_Independent - contacts_adj_mean_Democrat,
    mask_gap_dr = mask_adj_mean_Democrat - mask_adj_mean_Republican,
    mask_gap_di = mask_adj_mean_Democrat - mask_adj_mean_Independent
  )

write_csv(adj_behaviors_by_region, here("out", "tables", "adj_behaviors_by_region.csv"))
write_csv(adj_gaps_by_region, here("out", "tables", "adj_gaps_by_region.csv"))
write_csv(adj_vax_by_region, here("out", "tables", "adj_vax_by_region.csv"))
write_csv(adj_perception_by_region, here("out", "tables", "adj_perception_by_region.csv"))
cat("  Adjusted region estimates exported to adj_behaviors_by_region.csv, adj_gaps_by_region.csv, adj_vax_by_region.csv, adj_perception_by_region.csv\n")

# --- CENSUS DIVISION ADJUSTED ESTIMATES ---

bics_division <- bics_valid %>%
  filter(!is.na(census_division), !is.na(prev_week_mort_rate)) %>%
  mutate(
    log_mort = log(prev_week_mort_rate + 0.001),
    wave_factor = factor(wave)
  )

# Contacts model
contacts_division_model <- lm(num_cc ~ political_party * census_division +
                                wave_factor + log_mort,
                              data = bics_division,
                              weights = weight_party_raked)

# Mask model
mask_division_model <- lm(Norm_Masks_Used ~ political_party * census_division +
                            wave_factor + log_mort,
                          data = bics_division,
                          weights = weight_party_raked)

# Extract emmeans
contacts_division_emmeans <- emmeans(contacts_division_model, ~ political_party | census_division)
mask_division_emmeans <- emmeans(mask_division_model, ~ political_party | census_division)

# Convert to data frames
adj_behaviors_by_division <- bind_rows(
  as.data.frame(contacts_division_emmeans) %>%
    mutate(outcome = "contacts") %>%
    rename(estimate = emmean, adj_se = SE),
  as.data.frame(mask_division_emmeans) %>%
    mutate(outcome = "mask") %>%
    rename(estimate = emmean, adj_se = SE)
) %>%
  select(census_division, political_party, outcome, estimate, adj_se) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(estimate, adj_se),
    names_glue = "{outcome}_{.value}"
  ) %>%
  rename(
    contacts_adj_mean = contacts_estimate,
    contacts_adj_se = contacts_adj_se,
    mask_adj_mean = mask_estimate,
    mask_adj_se = mask_adj_se
  )

# Calculate adjusted gaps by division
adj_gaps_by_division <- adj_behaviors_by_division %>%
  select(census_division, political_party, contacts_adj_mean, mask_adj_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_adj_mean, mask_adj_mean)
  ) %>%
  mutate(
    contacts_gap_rd = contacts_adj_mean_Republican - contacts_adj_mean_Democrat,
    mask_gap_dr = mask_adj_mean_Democrat - mask_adj_mean_Republican
  )

write_csv(adj_behaviors_by_division, here("out", "tables", "adj_behaviors_by_division.csv"))
write_csv(adj_gaps_by_division, here("out", "tables", "adj_gaps_by_division.csv"))
cat("  Adjusted division estimates exported to adj_behaviors_by_division.csv, adj_gaps_by_division.csv\n")

# --- STATE-LEVEL ADJUSTED ESTIMATES ---

bics_state <- bics_valid %>%
  filter(!is.na(STATE), !is.na(prev_week_mort_rate)) %>%
  mutate(
    log_mort = log(prev_week_mort_rate + 0.001),
    wave_factor = factor(wave)
  )

# Contacts model
contacts_state_model <- lm(num_cc ~ political_party * STATE +
                             wave_factor + log_mort,
                           data = bics_state,
                           weights = weight_party_raked)

# Mask model
mask_state_model <- lm(Norm_Masks_Used ~ political_party * STATE +
                         wave_factor + log_mort,
                       data = bics_state,
                       weights = weight_party_raked)

# Extract emmeans
contacts_state_emmeans <- emmeans(contacts_state_model, ~ political_party | STATE)
mask_state_emmeans <- emmeans(mask_state_model, ~ political_party | STATE)

# Convert to data frames
adj_behaviors_by_state <- bind_rows(
  as.data.frame(contacts_state_emmeans) %>%
    mutate(outcome = "contacts") %>%
    rename(estimate = emmean, adj_se = SE),
  as.data.frame(mask_state_emmeans) %>%
    mutate(outcome = "mask") %>%
    rename(estimate = emmean, adj_se = SE)
) %>%
  select(STATE, political_party, outcome, estimate, adj_se) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(estimate, adj_se),
    names_glue = "{outcome}_{.value}"
  ) %>%
  rename(
    contacts_adj_mean = contacts_estimate,
    contacts_adj_se = contacts_adj_se,
    mask_adj_mean = mask_estimate,
    mask_adj_se = mask_adj_se
  )

# Calculate adjusted gaps by state (need both parties present)
adj_gaps_by_state <- adj_behaviors_by_state %>%
  select(STATE, political_party, contacts_adj_mean, mask_adj_mean) %>%
  pivot_wider(
    names_from = political_party,
    values_from = c(contacts_adj_mean, mask_adj_mean)
  ) %>%
  filter(!is.na(contacts_adj_mean_Republican) & !is.na(contacts_adj_mean_Democrat)) %>%
  mutate(
    contacts_gap_rd = contacts_adj_mean_Republican - contacts_adj_mean_Democrat,
    mask_gap_dr = mask_adj_mean_Democrat - mask_adj_mean_Republican
  )

write_csv(adj_behaviors_by_state, here("out", "tables", "adj_behaviors_by_state.csv"))
write_csv(adj_gaps_by_state, here("out", "tables", "adj_gaps_by_state.csv"))
cat("  Adjusted state estimates exported to adj_behaviors_by_state.csv, adj_gaps_by_state.csv\n")

# =============================================================================
# 6e. CONTACT DECOMPOSITION & ROBUSTNESS MODELS (Appendix M)
# =============================================================================
# Decompose contacts into household vs. non-household components and run
# outlier-robust models (negative binomial, quantile regression)

cat("Calculating contact decomposition and robustness models...\n")

# --- Contact decomposition by party and wave ---
contact_decomp <- bics_valid %>%
  filter(!is.na(num_cc), !is.na(num_cc_nonhh)) %>%
  group_by(political_party, wave, data_collected_dates) %>%
  summarise(
    n = n(),
    total_contacts = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    hh_contacts = total_contacts - nonhh_contacts,
    .groups = "drop"
  ) %>%
  mutate(
    political_party = factor(political_party, levels = c("Democrat", "Independent", "Republican"))
  ) %>%
  arrange(wave, political_party)

write_csv(contact_decomp, here("out", "tables", "contact_decomposition.csv"))
cat("  Contact decomposition exported to contact_decomposition.csv\n")

# --- Robustness models ---
# Prepare data: reuse model_data (already filtered for complete cases at line ~1282)
# Also need num_cc_nonhh to be non-missing
model_data_robust <- model_data %>%
  filter(!is.na(num_cc_nonhh))

# 1. Negative binomial regression for non-household contacts
library(MASS)

nb_model <- tryCatch({
  glm.nb(
    round(num_cc_nonhh) ~ political_party + wave + age_group + r_race +
      resp_hispanic + r_male + resp_educ + resp_hhsize +
      CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
    data = model_data_robust
  )
}, error = function(e) {
  cat("  Warning: Negative binomial model failed, falling back to Poisson\n")
  glm(
    round(num_cc_nonhh) ~ political_party + wave + age_group + r_race +
      resp_hispanic + r_male + resp_educ + resp_hhsize +
      CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
    data = model_data_robust,
    family = poisson()
  )
})

nb_coefs <- as.data.frame(summary(nb_model)$coefficients) %>%
  rownames_to_column("term") %>%
  filter(grepl("political_party", term)) %>%
  mutate(
    model = "Negative Binomial (non-HH contacts)",
    estimate_exp = exp(Estimate),
    ci_lower = exp(Estimate - 1.96 * `Std. Error`),
    ci_upper = exp(Estimate + 1.96 * `Std. Error`)
  )

# 2. Negative binomial for TOTAL contacts (for comparison)
nb_total_model <- tryCatch({
  glm.nb(
    round(num_cc) ~ political_party + wave + age_group + r_race +
      resp_hispanic + r_male + resp_educ + resp_hhsize +
      CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
    data = model_data_robust
  )
}, error = function(e) {
  glm(
    round(num_cc) ~ political_party + wave + age_group + r_race +
      resp_hispanic + r_male + resp_educ + resp_hhsize +
      CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
    data = model_data_robust,
    family = poisson()
  )
})

nb_total_coefs <- as.data.frame(summary(nb_total_model)$coefficients) %>%
  rownames_to_column("term") %>%
  filter(grepl("political_party", term)) %>%
  mutate(
    model = "Negative Binomial (total contacts)",
    estimate_exp = exp(Estimate),
    ci_lower = exp(Estimate - 1.96 * `Std. Error`),
    ci_upper = exp(Estimate + 1.96 * `Std. Error`)
  )

# 3. Quantile regressions for total contacts
library(quantreg)

qr_formula <- num_cc ~ political_party + wave + age_group + r_race +
  resp_hispanic + r_male + resp_educ + resp_hhsize +
  CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate

qr_results <- lapply(c(0.25, 0.50, 0.75), function(tau) {
  qr_fit <- rq(qr_formula, tau = tau, data = model_data_robust)
  qr_summ <- summary(qr_fit, se = "nid")
  coef_df <- as.data.frame(qr_summ$coefficients) %>%
    rownames_to_column("term") %>%
    filter(grepl("political_party", term))
  coef_df$model <- paste0("Quantile Regression (tau=", tau, ")")
  coef_df$tau <- tau
  # Standardize column names
  names(coef_df)[names(coef_df) == "Value"] <- "Estimate"
  names(coef_df)[names(coef_df) == "Std. Error"] <- "SE"
  names(coef_df)[names(coef_df) == "t value"] <- "t_value"
  names(coef_df)[names(coef_df) == "Pr(>|t|)"] <- "p_value"
  coef_df$ci_lower <- coef_df$Estimate - 1.96 * coef_df$SE
  coef_df$ci_upper <- coef_df$Estimate + 1.96 * coef_df$SE
  coef_df
})
qr_all <- bind_rows(qr_results)

# 4. OLS for non-household contacts (comparison)
ols_nonhh <- lm(
  num_cc_nonhh ~ political_party + wave + age_group + r_race +
    resp_hispanic + r_male + resp_educ + resp_hhsize +
    CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data_robust,
  weights = weight_party_raked
)

ols_nonhh_coefs <- as.data.frame(summary(ols_nonhh)$coefficients) %>%
  rownames_to_column("term") %>%
  filter(grepl("political_party", term)) %>%
  mutate(
    model = "OLS (non-HH contacts, weighted)"
  )

# Combine all robustness results
robustness_results <- bind_rows(
  nb_coefs %>% dplyr::select(term, model, Estimate, `Std. Error`,
                              estimate_exp, ci_lower, ci_upper),
  nb_total_coefs %>% dplyr::select(term, model, Estimate, `Std. Error`,
                                    estimate_exp, ci_lower, ci_upper),
  qr_all %>% dplyr::select(term, model, Estimate, SE, ci_lower, ci_upper, tau) %>%
    rename(`Std. Error` = SE),
  ols_nonhh_coefs %>% dplyr::select(term, model, Estimate, `Std. Error`) %>%
    mutate(ci_lower = Estimate - 1.96 * `Std. Error`,
           ci_upper = Estimate + 1.96 * `Std. Error`)
)

write_csv(robustness_results, here("out", "tables", "contact_robustness_models.csv"))
cat("  Robustness models exported to contact_robustness_models.csv\n")

# 5. Restricted sample: respondents with 2+ non-household contacts
# Tests whether gap persists when excluding low-contact respondents
model_data_2plus <- model_data_robust %>%
  filter(num_cc_nonhh >= 2)

ols_2plus <- lm(
  num_cc_nonhh ~ political_party + wave + age_group + r_race +
    resp_hispanic + r_male + resp_educ + resp_hhsize +
    CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data_2plus,
  weights = weight_party_raked
)

ols_2plus_coefs <- as.data.frame(summary(ols_2plus)$coefficients) %>%
  rownames_to_column("term") %>%
  filter(grepl("political_party", term)) %>%
  mutate(
    model = "OLS 2+ non-HH contacts (weighted)",
    ci_lower = Estimate - 1.96 * `Std. Error`,
    ci_upper = Estimate + 1.96 * `Std. Error`
  )

# Mask-usage OLS in 2+ non-HH sample
ols_mask_2plus <- lm(
  Norm_Masks_Used ~ political_party + wave + age_group + r_race +
    resp_hispanic + r_male + resp_educ + resp_hhsize +
    CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate,
  data = model_data_2plus,
  weights = weight_party_raked
)

ols_mask_2plus_coefs <- as.data.frame(summary(ols_mask_2plus)$coefficients) %>%
  rownames_to_column("term") %>%
  filter(grepl("political_party", term)) %>%
  mutate(
    model = "OLS mask usage 2+ non-HH (weighted)",
    ci_lower = Estimate - 1.96 * `Std. Error`,
    ci_upper = Estimate + 1.96 * `Std. Error`
  )

# Also compute descriptive means for 2+ sample (contacts + masks)
decomp_2plus <- bics_valid %>%
  filter(!is.na(num_cc), !is.na(num_cc_nonhh), num_cc_nonhh >= 2) %>%
  group_by(political_party, wave, data_collected_dates) %>%
  summarise(
    n = n(),
    total_contacts = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    nonhh_contacts = weighted.mean(num_cc_nonhh, w = weight_party_raked, na.rm = TRUE),
    hh_contacts = total_contacts - nonhh_contacts,
    mask_usage = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    political_party = factor(political_party, levels = c("Democrat", "Independent", "Republican"))
  ) %>%
  arrange(wave, political_party)

write_csv(decomp_2plus, here("out", "tables", "contact_decomposition_2plus.csv"))
write_csv(ols_2plus_coefs, here("out", "tables", "contact_robustness_2plus.csv"))
write_csv(ols_mask_2plus_coefs, here("out", "tables", "mask_robustness_2plus.csv"))
cat("  Restricted sample (2+ non-HH) exported to contact_decomposition_2plus.csv, contact_robustness_2plus.csv, mask_robustness_2plus.csv\n")
cat(sprintf("  N in 2+ non-HH sample: %d (%.1f%% of full model sample)\n",
            nrow(model_data_2plus), 100 * nrow(model_data_2plus) / nrow(model_data_robust)))

# =============================================================================
# 6h. BEHAVIORS BY CITY AND PARTY
# =============================================================================

cat("Calculating behaviors by city and party...\n")

behaviors_by_city <- bics_valid %>%
  filter(!is.na(city)) %>%
  group_by(city, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_party_raked^2 * (num_cc - weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    mask_se = sqrt(sum(weight_party_raked^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(behaviors_by_city, here("out", "tables", "behaviors_by_city.csv"))
cat("  Behaviors by city exported to behaviors_by_city.csv\n")

# =============================================================================
# 6i. BEHAVIORS BY OPPOSING-PARTY CD AND PARTY
# =============================================================================

cat("Calculating behaviors by opposing-party CD and party...\n")

behaviors_by_opposing_cd <- bics_valid %>%
  filter(!is.na(In_Opposing_Party_CD)) %>%
  mutate(opposing_cd = ifelse(In_Opposing_Party_CD == 1, "Opposing-party CD", "Same-party CD")) %>%
  group_by(opposing_cd, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    contacts_se = sqrt(sum(weight_party_raked^2 * (num_cc - weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
                  sum(weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    mask_se = sqrt(sum(weight_party_raked^2 * (Norm_Masks_Used - weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE))^2, na.rm = TRUE)) /
              sum(weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(behaviors_by_opposing_cd, here("out", "tables", "behaviors_by_opposing_cd.csv"))
cat("  Behaviors by opposing-party CD exported to behaviors_by_opposing_cd.csv\n")

# =============================================================================
# 6j. BEHAVIORS BY DEMOGRAPHIC/SES CATEGORIES AND PARTY
# =============================================================================

cat("Calculating behaviors by demographic/SES categories and party...\n")

# Helper function for grouped behavior summaries
calc_behaviors_by_group <- function(data, group_var, group_label) {
  data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(context = .data[[group_var]], political_party) %>%
    summarise(
      n = n(),
      contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
      mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(section = group_label, context = as.character(context))
}

# Age group
behaviors_by_age <- calc_behaviors_by_group(bics_valid, "age_group", "Age Group")

# Race
behaviors_by_race <- calc_behaviors_by_group(bics_valid, "r_race", "Race")

# Hispanic ethnicity
behaviors_by_hispanic <- calc_behaviors_by_group(bics_valid, "resp_hispanic", "Hispanic Ethnicity")

# Gender
behaviors_by_gender <- bics_valid %>%
  filter(!is.na(r_male)) %>%
  mutate(gender = ifelse(r_male == 1, "Male", "Female")) %>%
  group_by(context = gender, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(section = "Gender")

# Education
behaviors_by_education <- calc_behaviors_by_group(bics_valid, "resp_educ", "Education")

# Employment status
behaviors_by_employment <- calc_behaviors_by_group(bics_valid, "r_working", "Employment Status")

# Household size (binned)
behaviors_by_hhsize <- bics_valid %>%
  filter(!is.na(resp_hhsize)) %>%
  mutate(hhsize_cat = case_when(
    resp_hhsize <= 1 ~ "1 (Lives alone)",
    resp_hhsize == 2 ~ "2",
    resp_hhsize == 3 ~ "3",
    resp_hhsize >= 4 ~ "4+"
  )) %>%
  mutate(hhsize_cat = factor(hhsize_cat, levels = c("1 (Lives alone)", "2", "3", "4+"))) %>%
  group_by(context = hhsize_cat, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(section = "Household Size", context = as.character(context))

# Income (excluding prefer not to answer)
behaviors_by_income <- bics_valid %>%
  filter(!is.na(Income_Category), Income_Category != "Prefer not to answer") %>%
  mutate(Income_Category = factor(Income_Category, levels = c(
    "Less than $30,000", "$30,000 to $59,999", "$60,000 to $89,999", "$90,000 to $199,999"
  ))) %>%
  group_by(context = Income_Category, political_party) %>%
  summarise(
    n = n(),
    contacts_mean = weighted.mean(num_cc, w = weight_party_raked, na.rm = TRUE),
    mask_mean = weighted.mean(Norm_Masks_Used, w = weight_party_raked, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(section = "Household Income", context = as.character(context))

# Combine all demographic breakdowns
behaviors_by_demographics <- bind_rows(
  behaviors_by_age, behaviors_by_race, behaviors_by_hispanic,
  behaviors_by_gender, behaviors_by_education, behaviors_by_employment,
  behaviors_by_hhsize, behaviors_by_income
)

write_csv(behaviors_by_demographics, here("out", "tables", "behaviors_by_demographics.csv"))
cat("  Behaviors by demographics exported to behaviors_by_demographics.csv\n")

# =============================================================================
# 7. SAVE OUTPUT
# =============================================================================

# Save scalar statistics
output_path <- here("out", "tables", "paper_statistics.rds")
saveRDS(paper_stats, output_path)

# Save wave-level data for plotting
wave_data_path <- here("out", "tables", "behaviors_by_wave.csv")
write_csv(behaviors_by_wave, wave_data_path)

gaps_wave_path <- here("out", "tables", "gaps_by_wave.csv")
write_csv(gaps_by_wave, gaps_wave_path)

cat("\n=============================================================================\n")
cat("Paper statistics exported successfully!\n")
cat("Output files:\n")
cat("  - ", output_path, "\n")
cat("  - ", wave_data_path, "\n")
cat("  - ", gaps_wave_path, "\n")
cat("=============================================================================\n")

# Print summary
cat("\nKey statistics:\n")
cat(sprintf("  Sample: %s total (%s Dem, %s Rep, %s Ind)\n",
            format(paper_stats$n_total, big.mark = ","),
            format(paper_stats$n_dem, big.mark = ","),
            format(paper_stats$n_rep, big.mark = ","),
            format(paper_stats$n_ind, big.mark = ",")))
cat(sprintf("  Contacts gap (R-D): %.2f\n", paper_stats$contacts_gap))
cat(sprintf("  Mask gap (D-R): %.1f pp\n", paper_stats$mask_gap))
cat(sprintf("  Vaccination gap (D-R): %.1f pp\n", paper_stats$vax_gap))

cat("\nPandemic response (1 SD increase, standardized):\n")
cat("  Contacts ~ Incidence:\n")
cat(sprintf("    Dem=%.3f, Rep=%.3f, Ind=%.3f\n",
            paper_stats$contacts_inc_effect_dem,
            paper_stats$contacts_inc_effect_rep,
            paper_stats$contacts_inc_effect_ind))
cat("  Contacts ~ Mortality:\n")
cat(sprintf("    Dem=%.3f, Rep=%.3f, Ind=%.3f\n",
            paper_stats$contacts_mort_effect_dem,
            paper_stats$contacts_mort_effect_rep,
            paper_stats$contacts_mort_effect_ind))
cat("  Vaccination ~ Incidence (pp):\n")
cat(sprintf("    Dem=%.2f, Rep=%.2f, Ind=%.2f\n",
            paper_stats$vax_inc_effect_dem,
            paper_stats$vax_inc_effect_rep,
            paper_stats$vax_inc_effect_ind))
cat("  Vaccination ~ Mortality (pp):\n")
cat(sprintf("    Dem=%.2f, Rep=%.2f, Ind=%.2f\n",
            paper_stats$vax_mort_effect_dem,
            paper_stats$vax_mort_effect_rep,
            paper_stats$vax_mort_effect_ind))

cat("\nWave-level data:\n")
print(behaviors_by_wave)
