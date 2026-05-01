# =============================================================================
# 07 — Competing-explanations coefficient-stability plot
#
# Visualizes how the Republican-Democrat partisan coefficient changes as
# progressively adding each competing-explanation control, then a full
# multivariate model. 8 models total (Base + 6 competing-explanation
# additions + Full).
#
# Output: out/plots/competing_explanations.png
#         out/tables/competing_explanations.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(here)
  library(cowplot)
  library(patchwork)
})

source(here("code", "00a-Create_Party_Weights.R"))

# ---- Census region from STATE -----------------------------------------------
northeast <- c("Connecticut","Maine","Massachusetts","New Hampshire","New Jersey",
               "New York","Pennsylvania","Rhode Island","Vermont")
midwest   <- c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota",
               "Missouri","Nebraska","North Dakota","Ohio","South Dakota","Wisconsin")
south     <- c("Alabama","Arkansas","Delaware","District of Columbia","Florida",
               "Georgia","Kentucky","Louisiana","Maryland","Mississippi",
               "North Carolina","Oklahoma","South Carolina","Tennessee","Texas",
               "Virginia","West Virginia")
west      <- c("Alaska","Arizona","California","Colorado","Hawaii","Idaho",
               "Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")

bics <- bics_zip_features %>%
  mutate(
    census_region = case_when(
      STATE %in% northeast ~ "Northeast",
      STATE %in% midwest   ~ "Midwest",
      STATE %in% south     ~ "South",
      STATE %in% west      ~ "West",
      TRUE ~ NA_character_
    ),
    political_party = factor(political_party,
                             levels = c("Democrat","Independent","Republican")),
    wave = factor(wave)
  )

# Predictors used across all 8 model specs (same for both outcomes)
predictors_needed <- c("political_party","wave",
            "county_mask_mandate","age_group","r_race","r_male","resp_educ",
            "resp_hhsize","r_working","census_region","Metro",
            "CD_PERCENT_DEMOCRAT","log_prev_week_mort_rate",
            "binary_concern_strong","weight_party_raked")

# Per-outcome samples — outcome's missingness shouldn't constrain the other panel
# (Norm_Masks_Used is structurally missing for ~29% who had no eligible contacts
#  to mask during; that affects masks models only.)
contacts_sample <- bics %>%
  filter(political_party %in% c("Democrat","Independent","Republican"),
         !is.na(num_cc),
         if_all(all_of(predictors_needed), ~ !is.na(.)))

masks_sample <- bics %>%
  filter(political_party %in% c("Democrat","Independent","Republican"),
         !is.na(Norm_Masks_Used),
         if_all(all_of(predictors_needed), ~ !is.na(.)))

cat("Contacts-sample N:", nrow(contacts_sample), "\n")
cat("Masks-sample N:   ", nrow(masks_sample), "\n")

# ---- Define 8 model specifications ------------------------------------------
specs <- tribble(
  ~label,                ~rhs,
  "1. Base (party + wave)",          "political_party + wave",
  "2. + Policy (mask mandate)",      "political_party + wave + county_mask_mandate",
  "3. + Demographics",               "political_party + wave + age_group + r_race + r_male + resp_educ + resp_hhsize + r_working",
  "4. + Geographic region",          "political_party + wave + census_region + Metro",
  "5. + Cross-cutting context",      "political_party + wave + CD_PERCENT_DEMOCRAT",
  "6. + Severity (local mortality)", "political_party + wave + log_prev_week_mort_rate",
  "7. + Perception (concern)",       "political_party + wave + binary_concern_strong",
  "8. Full multivariate",            "political_party + wave + county_mask_mandate + age_group + r_race + r_male + resp_educ + resp_hhsize + r_working + census_region + Metro + CD_PERCENT_DEMOCRAT + log_prev_week_mort_rate + binary_concern_strong"
)

run_one <- function(label, rhs, outcome, dat) {
  fit <- lm(as.formula(paste(outcome, "~", rhs)),
            data = dat, weights = weight_party_raked)
  tidy(fit) %>%
    filter(term == "political_partyRepublican") %>%
    transmute(model = label,
              outcome = outcome,
              estimate = estimate,
              std_error = std.error,
              p_value = p.value,
              n = nobs(fit))
}

results <- bind_rows(
  map2_dfr(specs$label, specs$rhs, ~ run_one(.x, .y, "num_cc", contacts_sample)),
  map2_dfr(specs$label, specs$rhs, ~ run_one(.x, .y, "Norm_Masks_Used", masks_sample))
) %>%
  mutate(model = factor(model, levels = rev(specs$label)))   # top of plot = Base

write_csv(results, here("out","tables","competing_explanations.csv"))
print(results)

# ---- Plot -------------------------------------------------------------------
plot_data <- results %>%
  mutate(
    is_anchor = grepl("^1\\.|^8\\.", model),
    role = case_when(
      grepl("^1\\.", model) ~ "Base",
      grepl("^8\\.", model) ~ "Full",
      TRUE                  ~ "Single addition"
    ),
    role = factor(role, levels = c("Base","Single addition","Full")),
    lo = estimate - 1.96*std_error,
    hi = estimate + 1.96*std_error
  )

role_palette <- c("Base" = "gray40",
                  "Single addition" = "#2c7fb8",
                  "Full" = "#c50000")

# Contacts panel
contact_data <- plot_data %>% filter(outcome == "num_cc")
base_c <- contact_data %>% filter(role == "Base") %>% pull(estimate)

p_contact <- ggplot(contact_data, aes(x = estimate, y = model, color = role)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = base_c, linetype = "dotted", color = "gray40") +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.5, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f", estimate)),
            vjust = -0.9, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = role_palette, name = NULL) +
  labs(x = "Republican - Democrat gap (contacts/day)", y = NULL,
       title = "A. Contacts") +
  theme_cowplot(11) +
  theme(plot.title = element_text(face = "bold", size = 11),
        legend.position = "none")

# Masks panel — flip sign so positive = Dems higher (puts 0 on the left like contacts)
mask_data <- plot_data %>%
  filter(outcome == "Norm_Masks_Used") %>%
  mutate(estimate_dr = -estimate,
         lo_dr = -hi,
         hi_dr = -lo)
base_m <- mask_data %>% filter(role == "Base") %>% pull(estimate_dr)

p_mask <- ggplot(mask_data, aes(x = estimate_dr, y = model, color = role)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = base_m, linetype = "dotted", color = "gray40") +
  geom_pointrange(aes(xmin = lo_dr, xmax = hi_dr), size = 0.5, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.1f", estimate_dr)),
            vjust = -0.9, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = role_palette, name = NULL) +
  labs(x = "Democrat - Republican gap (mask usage, pp)", y = NULL,
       title = "B. Mask usage") +
  theme_cowplot(11) +
  theme(plot.title = element_text(face = "bold", size = 11),
        axis.text.y = element_blank(),
        legend.position = "right")

combined <- p_contact + p_mask +
  plot_layout(widths = c(1.3, 1)) +
  plot_annotation(
    title = "Partisan Coefficient Stability Across Competing-Explanation Controls",
    caption = paste0(
      "Republican-Democrat coefficient from weighted OLS at each model specification (raked party weights). ",
      "Contacts panel N = ", format(nrow(contacts_sample), big.mark = ","),
      "; Masks panel N = ", format(nrow(masks_sample), big.mark = ","),
      ". Whiskers are 95% CIs. Dotted vertical line marks the base-model coefficient; ",
      "if any single competing explanation accounted for the partisan gap, that model's coefficient ",
      "would shift toward zero. Models 2-7 each add one competing-explanation block; Model 8 includes all controls."
    )
  ) &
  theme(plot.caption = element_text(size = 8, color = "gray30",
                                    hjust = 0, lineheight = 1.1))

out_path <- here("out","plots","competing_explanations.png")
ggsave(out_path, combined, width = 13, height = 6, dpi = 200, bg = "white")

cat("\nSaved plot to:", out_path, "\n")
cat("Saved table to: out/tables/competing_explanations.csv\n")
