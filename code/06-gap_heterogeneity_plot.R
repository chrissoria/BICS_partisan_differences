# =============================================================================
# 06 — Gap heterogeneity plot
# Forest-style plot of Republican-Democrat gaps in contacts and mask usage
# across many subgroup dimensions (region, metro, mandate, CD partisanship,
# age, education, race, gender). Within each subgroup level, the gap is
# wave-adjusted with raked party weights.
#
# Output: out/plots/gap_heterogeneity.png
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

bics <- bics_zip_features %>%
  mutate(
    census_region = case_when(
      STATE %in% northeast ~ "Northeast",
      STATE %in% midwest   ~ "Midwest",
      STATE %in% south     ~ "South",
      STATE %in% west      ~ "West",
      TRUE ~ NA_character_
    ),
    metro_label = case_when(
      Metro == 1 ~ "Metro",
      Metro == 0 ~ "Non-Metro",
      TRUE ~ NA_character_
    ),
    cd_label = case_when(
      CD_PERCENT_DEMOCRAT > 50 ~ "Dem-majority CD",
      CD_PERCENT_DEMOCRAT < 50 ~ "Rep-majority CD",
      TRUE ~ NA_character_
    ),
    race_simple = case_when(
      r_race == "White" ~ "White",
      !is.na(r_race) ~ "Non-White",
      TRUE ~ NA_character_
    ),
    gender_label = case_when(
      r_male == 1 ~ "Male",
      r_male == 0 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(political_party %in% c("Democrat", "Independent", "Republican"),
         !is.na(weight_party_raked), !is.na(wave),
         !is.na(num_cc), !is.na(Norm_Masks_Used)) %>%
  mutate(political_party = factor(political_party,
                                  levels = c("Democrat", "Independent", "Republican")))

# ---- Helper: unadjusted weighted Rep-Dem gap with SE ------------------------
# Simple weighted mean difference (R - D) within the subgroup, no wave FE.
# Implemented via lm(y ~ party, weights = w): coefficient is the weighted
# mean difference, SE is the OLS standard error under weights.
compute_gap <- function(data, dimension, level) {
  if (sum(data$political_party == "Republican") < 50 ||
      sum(data$political_party == "Democrat") < 50) return(NULL)

  m_contact <- lm(num_cc ~ political_party,
                  data = data, weights = weight_party_raked)
  m_mask <- lm(Norm_Masks_Used ~ political_party,
               data = data, weights = weight_party_raked)

  c_coef <- broom::tidy(m_contact) %>% filter(term == "political_partyRepublican")
  m_coef <- broom::tidy(m_mask) %>% filter(term == "political_partyRepublican")

  tibble(
    dimension = dimension,
    level = level,
    n = nrow(data),
    contacts_gap = c_coef$estimate, contacts_se = c_coef$std.error,
    mask_gap = m_coef$estimate,     mask_se = m_coef$std.error
  )
}

# ---- Iterate over subgroups -------------------------------------------------
specs <- list(
  list(dim = "Region",        var = "census_region",
       levels = c("Northeast","Midwest","South","West")),
  list(dim = "Metro",         var = "metro_label",
       levels = c("Metro","Non-Metro")),
  list(dim = "Mask mandate",  var = "county_mask_mandate",
       levels = c("None","Less Strict","Strict")),
  list(dim = "Cong. district",var = "cd_label",
       levels = c("Dem-majority CD","Rep-majority CD")),
  list(dim = "Age",           var = "age_group",
       levels = c("[18,30)","[30,45)","[45,55)","[55,65)","[65,100]")),
  list(dim = "Education",     var = "resp_educ",
       levels = c("Non-high school graduate","High school graduate",
                  "Some college","College graduate")),
  list(dim = "Race",          var = "race_simple",
       levels = c("White","Non-White")),
  list(dim = "Gender",        var = "gender_label",
       levels = c("Female","Male"))
)

results <- map_dfr(specs, function(spec) {
  map_dfr(spec$levels, function(lv) {
    sub <- bics %>% filter(.data[[spec$var]] == lv)
    compute_gap(sub, spec$dim, lv)
  })
})

write_csv(results, here("out","tables","gap_heterogeneity.csv"))

# ---- Plot -------------------------------------------------------------------
# Lock dimension display order (top to bottom on the y-axis)
dim_order <- c("Region","Metro","Mask mandate","Cong. district",
               "Age","Education","Race","Gender")

plot_data <- results %>%
  mutate(
    contacts_lo = contacts_gap - 1.96 * contacts_se,
    contacts_hi = contacts_gap + 1.96 * contacts_se,
    mask_gap_dr = -mask_gap,
    mask_lo_dr  = -(mask_gap + 1.96 * mask_se),
    mask_hi_dr  = -(mask_gap - 1.96 * mask_se),
    dimension = factor(dimension, levels = dim_order)
  )

dim_palette <- c(
  "Region" = "#1f77b4", "Metro" = "#2ca02c", "Mask mandate" = "#ff7f0e",
  "Cong. district" = "#9467bd", "Age" = "#8c564b", "Education" = "#e377c2",
  "Race" = "#7f7f7f", "Gender" = "#17becf"
)

# Within-dimension sort, panel-specific
contacts_data <- plot_data %>%
  group_by(dimension) %>%
  mutate(level = fct_reorder(level, contacts_gap)) %>%
  ungroup()

masks_data <- plot_data %>%
  group_by(dimension) %>%
  mutate(level = fct_reorder(level, mask_gap_dr)) %>%
  ungroup()

p_contacts <- ggplot(contacts_data,
                     aes(x = contacts_gap, y = level, color = dimension)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = contacts_lo, xmax = contacts_hi),
                  size = 0.4, linewidth = 0.6) +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_color_manual(values = dim_palette, guide = "none") +
  labs(x = "R - D gap (contacts/day)", y = NULL,
       title = "A. Contacts (Republican - Democrat)") +
  theme_cowplot(12) +
  theme(plot.title = element_text(face = "bold", size = 11),
        strip.placement = "outside",
        strip.background = element_rect(fill = "gray95", color = NA),
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 10),
        panel.spacing.y = unit(0.3, "lines"),
        panel.border = element_rect(color = "gray85", fill = NA))

p_masks <- ggplot(masks_data,
                  aes(x = mask_gap_dr, y = level, color = dimension)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = mask_lo_dr, xmax = mask_hi_dr),
                  size = 0.4, linewidth = 0.6) +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = dim_palette, guide = "none") +
  labs(x = "D - R gap (mask usage, pp)", y = NULL,
       title = "B. Mask usage (Democrat - Republican)") +
  theme_cowplot(12) +
  theme(plot.title = element_text(face = "bold", size = 11),
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing.y = unit(0.3, "lines"),
        panel.border = element_rect(color = "gray85", fill = NA))

combined <- p_contacts + p_masks +
  plot_annotation(
    title = "Partisan Gap Across Subgroups",
    caption = "Republican-Democrat gap in daily contacts (Panel A) and Democrat-Republican gap in mask usage (Panel B), within each subgroup level. Gaps are weighted mean differences using raked party weights, with no wave or demographic controls. Whiskers are 95% CIs. Within each dimension, levels are sorted by gap size."
  ) &
  theme(plot.caption = element_text(size = 8, color = "gray30",
                                    hjust = 0, lineheight = 1.1))

out_path <- here("out", "plots", "gap_heterogeneity.png")
ggsave(out_path, combined, width = 12, height = 7, dpi = 200, bg = "white")

cat("Saved plot to:", out_path, "\n")
cat("Saved table to: out/tables/gap_heterogeneity.csv\n")
