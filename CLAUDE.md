# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This project analyzes partisan differences in health behaviors during the COVID-19 pandemic using data from the Berkeley Interpersonal Contact Study (BICS). It includes both statistical analysis of survey data and a Quarto-based academic paper.

## Target Publications

1. **Journal of Health and Social Behavior (JHSB)** - First choice; ASA health specialty journal; ~10,000 words
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

### Research Questions
1. Are partisan differences in health behaviors robust to policy, geography, and demographics?
2. Under what conditions are partisan gaps largest vs. smallest?

## Directory Structure

```
BICS_partisan_differences/
├── code/                    # Analysis scripts (run in numbered order)
│   ├── 00-Prep_BICS_Data.Rmd           # Data preparation (run first); outputs BICS_ZIP_Features.csv
│   ├── 00a-Create_Party_Weights.R      # Creates raked party weights (sourced by export script)
│   ├── 01-descriptive_tables_plots.Rmd # Descriptive statistics; requires plot_helper_functions/
│   ├── 01a-Weight_Sensitivity_Analysis.R # Weight sensitivity analysis
│   ├── 02-differences.Rmd              # Partisan differences (demographic adjustment); requires plot_helper_functions/
│   ├── 03-waves_pooled_behavior.Rmd    # Pooled behavior regressions; requires plot_helper_functions/
│   ├── 04-opposing_districts_waves_pooled_behavior.Rmd  # Cross-cutting context tests
│   ├── 05-incidence_mortality_response.Rmd              # Pandemic severity response
│   └── export_paper_statistics.R       # Exports all stats to paper_statistics.rds
├── data/                    # Raw and processed data files
│   └── BICS_ZIP_Features.csv           # Main analysis dataset (output of 00-Prep)
├── out/                     # Output files
│   ├── plots/
│   └── tables/
│       └── paper_statistics.rds        # Pre-calculated paper statistics (single source of truth)
└── paper/                   # Quarto paper
    ├── partisan_differences.qmd        # Main document (APA citations, quarto defaults)
    ├── sections/                       # Modular sections included by main document
    │   ├── introduction.qmd
    │   ├── methods.qmd
    │   ├── results.qmd                 # Loads paper_statistics.rds via list2env()
    │   ├── discussion.qmd
    │   ├── conclusion.qmd
    │   └── appendix.qmd
    ├── references.bib
    └── jhsb_submission/                # JHSB-formatted version (ASA citations, anonymized)
        ├── CLAUDE.md                   # JHSB formatting requirements
        ├── partisan_differences.qmd
        ├── american-sociological-association.csl
        ├── references.bib
        └── sections/                   # Parallel section files with JHSB-specific edits
```

## Analysis Pipeline

Run scripts in this order:

1. `00-Prep_BICS_Data.Rmd` - Prepares data, creates `weight_party_raked`, outputs `data/BICS_ZIP_Features.csv`
2. `export_paper_statistics.R` - Auto-sources `00a-Create_Party_Weights.R` if needed; outputs `out/tables/paper_statistics.rds`
3. Other analysis scripts (01–05) as needed
4. Render paper

## Paper Statistics Workflow

**Key principle:** The paper reads pre-calculated statistics from `out/tables/paper_statistics.rds` rather than recalculating values. This ensures consistency between analysis code and paper.

`code/export_paper_statistics.R` calculates all key statistics and saves them. `paper/sections/results.qmd` loads them:

```r
paper_stats <- readRDS("../out/tables/paper_statistics.rds")
list2env(paper_stats, envir = environment())
```

Inline R code then references statistics directly:
```
Republicans reported `r sprintf("%.1f", contacts_rep)` contacts...
```

**To update paper statistics:** Re-run `code/export_paper_statistics.R`

## Weighting

- `weight_pooled` - Original demographic weights (age, gender, race, education)
- `weight_party_raked` - Raked weights that also adjust for party ID to match Gallup 2020 targets (30% Dem, 29% Rep, 39% Ind)

All main analysis scripts use `weight_party_raked`. The weight sensitivity analysis (`01a-Weight_Sensitivity_Analysis.R`) compares results across weighting approaches.

## Key Variables

**Outcome variables:**
- `num_cc` - Total daily contacts
- `num_cc_nonhh` - Non-household contacts
- `log_num_cc` - Log-transformed contacts
- `num_cc_50_max`, `num_cc_25_max`, `num_cc_15_max` - Capped contact counts (outlier robustness)
- `Norm_Masks_Used` - Mask usage (0–100 scale)
- `vax_acceptance` - Vaccinated OR intends to vaccinate (wave 6 only)
- `Vaccinated` - Vaccination status only (wave 6 only)
- `will_get_vax` - Intends to get vaccinated (unvaccinated respondents)

**Predictors and context:**
- `political_party` - Democrat, Republican, Independent
- `republican` - Binary indicator
- `CD_PERCENT_DEMOCRAT` - Congressional district partisanship (opposing-context variable)
- `political_party_to_CD` - 4-level interaction (Dem/Rep × Dem CD/Rep CD) for script 04
- `county_mask_mandate` - Strict / Less Strict / Unspecified / None
- `COUNTY_RUCC_2013` - Urban/rural classification
- `log_prev_week_inc_rate`, `log_prev_week_mort_rate` - Logged pandemic severity (lagged)
- `binary_concern`, `binary_concern_strong` - Perceived severity indicators

## Model Specification Strategy

Scripts 03–05 use **progressive nested models** to show partisan gaps persist across all specifications:
- **Model 1**: Base (party + wave FEs)
- **Model 2**: + Demographic controls (age, race, gender, education, employment, household size)
- **Model 3**: + Contextual controls (local incidence/mortality, CD partisanship)

This structure directly tests Theories 1–5 above: if gaps disappear in later models, the theory is supported; if they persist, partisan identity overrides those explanations.

## Common Commands

```r
# Run data prep
rmarkdown::render("code/00-Prep_BICS_Data.Rmd")

# Export paper statistics
source("code/export_paper_statistics.R")

# Render main paper
quarto render paper/partisan_differences.qmd

# Render JHSB submission (paths use ../../out/tables/ from that subdirectory)
quarto render paper/jhsb_submission/partisan_differences.qmd --to pdf
```

## JHSB Submission Notes

See `paper/jhsb_submission/CLAUDE.md` for full formatting requirements. Key differences from main paper:
- ASA citation style (not APA); uses `american-sociological-association.csl`
- No heading on Introduction; "Methods" → "Data and Methods"
- Footnotes converted to a "NOTES" section (not inline)
- Abstract < 150 words; title page anonymized; ≤ 10,000 words total
- Paths to output files use `../../out/tables/` (one level deeper than main paper)
