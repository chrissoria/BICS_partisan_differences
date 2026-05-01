# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This directory contains the **Social Science & Medicine (SSM)** submission. See the root `CLAUDE.md` for the broader project (analysis pipeline, theoretical framing, variable definitions) and `paper/jhsb_submission/CLAUDE.md` for the parallel JHSB version.

## Current Scope

This directory currently holds **only the abstract** (`abstract.qmd` → `abstract.docx`, plus `SSM_abstract.docx`). The full manuscript has not been ported here yet — only the abstract is being prepared for SSM submission. Recent work focused on framing and tightening the abstract (commits 5ed89a1, f712731).

## Files

```
SSM_submission/
├── abstract.qmd          # Source: title page + structured abstract (Intro/Methods/Results/Discussion)
├── abstract.docx         # Rendered output
├── SSM_abstract.docx     # Submission copy
├── references.bib        # Bibliography (mirrors paper/references.bib)
└── wordcount.lua         # Quarto filter for word counting
```

## Path Convention

Like `jhsb_submission/`, this directory is **two levels deep** from the project root, so the abstract reads pre-calculated statistics from `../../out/tables/`:

```r
paper_stats <- readRDS("../../out/tables/paper_statistics.rds")
adj_gaps    <- read_csv("../../out/tables/adjusted_gaps_by_wave.csv")
reg_gaps    <- read_csv("../../out/tables/adj_gaps_by_region.csv")
```

If a statistic referenced in the abstract is missing, regenerate it via `code/export_paper_statistics.R` from the project root.

## Rendering

```bash
quarto render paper/SSM_submission/abstract.qmd
```

Output is `.docx` (SSM accepts Word). The `wordcount.lua` filter reports word count after rendering.

## Abstract Structure

SSM uses a **structured abstract** with four labeled sections: Introduction, Methods, Results, Discussion. The current draft includes:
- Author block with affiliations (UC Berkeley Demography; UW Sociology) — **not anonymized** (SSM uses single-blind review by default)
- One inline figure (`contrast_contacts_masks.png`) embedded in Results
- Inline R for all numeric statistics — never hard-code numbers; always pull from the loaded `paper_stats` / CSV objects so the abstract stays in sync with analysis output

## Citation Style

SSM uses a Vancouver-style numbered system in the published version, but for submission Quarto's default (or APA via `apa.csl` from the main paper) is acceptable — the journal will reformat. Do not switch to ASA style here (that is JHSB-specific).

## When Extending to a Full Manuscript

If/when the full SSM manuscript is built out in this directory, mirror the structure of `jhsb_submission/` (a `partisan_differences.qmd` with `sections/` includes), but note SSM-specific requirements:
- Word limit: ~8,000–10,000 words (check current Guide for Authors)
- Highlights: 3–5 bullets, ≤ 85 characters each (required at submission)
- Structured abstract ≤ 300 words (longer than JHSB's 150)
- Declarations: funding, competing interests, author contributions (CRediT taxonomy)
