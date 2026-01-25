# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project Overview

This project analyzes partisan differences in health behaviors during the COVID-19 pandemic using data from the Berkeley Interpersonal Contact Study (BICS). It includes both statistical analysis of survey data and a Quarto-based academic paper.

## Target Publications

1. **Journal of Health and Social Behavior (JHSB)** - First choice; ASA health specialty journal; ~8,000 words
2. **Social Science & Medicine** - Interdisciplinary; generous word limit (~8,000-10,000)
3. **Sociological Science** - Fast review; open access; growing prestige
4. **SSM - Population Health** - Newer journal; good fit for population health disparities

## Paper Framing and Theoretical Approach

### The Puzzle
- County-level mortality disparities by partisanship (Republican counties had higher COVID mortality)
- This is MOTIVATIONAL only—we don't make causal claims from aggregate data (avoid ecological fallacy)
- Individual-level BICS data tests whether behavioral differences exist at individual level

### Core Argument
- Partisan identity has become a fundamental social identity (like race, religion)
- Identity drives behavior, not just correlates with it
- Health became politicized during COVID → expect behavioral divergence
- We use descriptive patterns to adjudicate between competing explanations

### Competing Theories (and their testable predictions)

| Theory | If True, We Should See... |
|--------|---------------------------|
| 1. "It's just policy" | Gaps disappear within same policy environments |
| 2. "It's just demographics" | Gaps disappear after demographic controls |
| 3. "Social conformity" | People shift toward local norms regardless of party |
| 4. "Cross-cutting contexts reduce polarization" | Gaps smaller in opposing-party areas |
| 5. "Context vs. identity" | If identity dominates: gaps persist WITHIN all contexts |
| 6. "Abstraction enables polarization" | Gaps narrow during high-severity periods |
| 7. "Information/perception" | Gaps disappear after controlling for perceived severity |

### Key Framing Decisions
- **Descriptive, not causal**: We can't test mechanisms directly, but patterns constrain plausible explanations
- **"Portable polarization"**: If gaps persist across contexts, identity overrides local culture
- **Perception as performative**: Stated perceptions may be expressive (what group members "should" say)

### Data Strengths to Emphasize
- **Actual contact counts** (not abstract categorical questions)
- **Mask usage during specific contacts** (behavioral, not attitudinal)
- Relevant for: polarization researchers, disease modelers, heterogeneity in population dynamics
- Heterogeneity matters: population averages can obscure variation that shifts disease outcomes

### Research Questions
1. Are partisan differences in health behaviors robust to policy, geography, and demographics?
2. Under what conditions are partisan gaps largest vs. smallest?

## Directory Structure

```
BICS_partisan_differences/
├── code/                    # Analysis scripts
│   ├── 00-Prep_BICS_Data.Rmd           # Data preparation (run first)
│   ├── 00a-Create_Party_Weights.R      # Creates raked party weights
│   ├── 01-descriptive_tables_plots.Rmd # Descriptive statistics
│   ├── 01a-Weight_Sensitivity_Analysis.R # Weight sensitivity analysis
│   ├── 02-differences.Rmd              # Partisan differences analysis
│   ├── 03-waves_pooled_behavior.Rmd    # Pooled behavior regressions
│   ├── 04-opposing_districts_waves_pooled_behavior.Rmd
│   ├── 05-incidence_mortality_response.Rmd
│   └── export_paper_statistics.R       # Exports stats for paper
├── data/                    # Data files
│   └── BICS_ZIP_Features.csv           # Main analysis dataset
├── out/                     # Output files
│   ├── plots/
│   └── tables/
│       ├── paper_statistics.rds        # Pre-calculated paper statistics
│       ├── weight_sensitivity_summary.csv
│       └── regression_weight_sensitivity.csv
└── paper/                   # Quarto paper
    ├── partisan_differences.qmd        # Main document
    ├── sections/                       # Paper sections (included in main)
    │   ├── introduction.qmd
    │   ├── methods.qmd
    │   ├── results.qmd
    │   ├── discussion.qmd
    │   └── appendix.qmd
    └── references.bib
```

## Analysis Pipeline

Run scripts in this order:

1. `00-Prep_BICS_Data.Rmd` - Prepares data, creates `weight_party_raked`
2. `export_paper_statistics.R` - Exports statistics for paper
3. Other analysis scripts (01-05) as needed
4. Render paper with `quarto render paper/partisan_differences.qmd`

## Paper Statistics Workflow

**Key principle:** The paper reads pre-calculated statistics from `out/tables/paper_statistics.rds` rather than recalculating values. This ensures:
- Consistency between analysis code and paper
- Faster paper rendering
- Single source of truth for all statistics

**How it works:**

1. `code/export_paper_statistics.R` calculates all key statistics:
   - Sample sizes and percentages
   - Demographics by party (age, gender, race, metro, education, employment)
   - Health behaviors (contacts, mask usage, vaccination)
   - Partisan gaps

2. Statistics are saved to `out/tables/paper_statistics.rds`

3. `paper/sections/results.qmd` loads and uses these statistics:
   ```r
   paper_stats <- readRDS("../out/tables/paper_statistics.rds")
   list2env(paper_stats, envir = environment())
   ```

4. Inline R code references the statistics:
   ```
   Republicans reported `r sprintf("%.1f", contacts_rep)` contacts...
   ```

**To update paper statistics:** Re-run `code/export_paper_statistics.R`

## Weighting

The project uses two weight variables:

- `weight_pooled` - Original demographic weights (age, gender, race, education)
- `weight_party_raked` - Raked weights that also adjust for party ID to match Gallup 2020 targets (30% Dem, 29% Rep, 39% Ind)

All main analysis scripts use `weight_party_raked`. The weight sensitivity analysis (`01a-Weight_Sensitivity_Analysis.R`) compares results across weighting approaches.

## Key Variables

- `num_cc` - Total daily contacts
- `num_cc_nonhh` - Non-household contacts
- `Norm_Masks_Used` - Mask usage (0-100 scale)
- `vax_acceptance` - Vaccine acceptance: vaccinated OR intends to vaccinate (wave 6 only)
- `Vaccinated` - Vaccination status only (wave 6 only)
- `will_get_vax` - Intends to get vaccinated among unvaccinated
- `political_party` - Democrat, Republican, Independent

## Common Commands

```r
# Render paper
quarto render paper/partisan_differences.qmd

# Export paper statistics
source("code/export_paper_statistics.R")

# Run data prep
rmarkdown::render("code/00-Prep_BICS_Data.Rmd")
```
