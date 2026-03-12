# CLAUDE.md — JHSB Submission

This directory contains the version of the paper tailored for submission to the **Journal of Health and Social Behavior (JHSB)**.

## Submission Requirements

### Length
- **10,000 words maximum** for regular manuscripts
- Word count **includes**: title page, abstract, text, notes, references
- Word count **excludes**: tables and figures (but lengthy/horizontal tables affect page count)

### Formatting
- Double-spaced throughout (including notes and references)
- Times New Roman, 12pt
- Margins: at least 1 inch (line length ≤ 6.5 inches)

### Title Page (Page 1)
- **Anonymized** for submission (no author names, affiliations, acknowledgments, grants)
- Must include:
  - Title
  - Total word count (text + notes + references; NOT tables/figures)
  - Number of tables
  - Number of figures
  - Running head (short title, < 55 characters with spaces)

### Abstract
- **< 150 words**
- Must include: sample size, study design, data source

### Section Headings (ASA style)
- **No heading for the introduction** — text begins with the full title
- Required major headings: **Background, Data and Methods, Results, Discussion, References**
- Up to three heading levels

### Citations (ASA Style — NOT APA)
- Author-date in text: `Duncan (1959)` or `(Gouldner 1963)`
- Pages after colon: `(Ramirez and Weiss 1979:239–40)`
- Two authors: list both names always
- Three authors: all three on first cite, then `et al.` after
- Four+ authors: `et al.` throughout
- Series: alphabetical or date order, semicolons: `(Burgess 1968; Marwell et al. 1971)`
- No "I" self-citations — use third person

### Notes
- Superscript Arabic numerals in text
- Collected in a separate "NOTES" section after text, before references
- Do NOT use Word/Quarto footnote functions — notes go in their own section
- Keep notes short

### References
- Separate "REFERENCES" section after notes
- Alphabetical by last name; full first names and middle initials
- All authors listed (no `et al.` in reference list)
- ASA format (see submission guidelines for examples)

### Tables
- Numbered consecutively
- Each on a separate page
- Descriptive title + column/row headings (no abbreviations)
- Align numbers by decimal
- General notes as "Note:"; footnotes as a, b, c
- Significance: `*` p < .05, `**` p < .01, `***` p < .001 (specify one-tailed or two-tailed)
- Must be editable (not images)

### Figures
- Numbered consecutively
- Each includes title/caption (no abbreviations)
- Must be legible at column width (2-9/16") or full page width (5-5/16")
- Black ink / computer-generated

### Appendices
- Lettered (A, B, C), not numbered
- Descriptive title: e.g., "APPENDIX A. Variable Names and Definitions"

## Key Differences from Main Paper Version

The main paper (in `paper/`) uses APA citation style and Quarto defaults. This version must:

1. Switch from APA to ASA citation style (need `american-sociological-association.csl`)
2. Rename "Introduction" → no heading (or "Background")
3. Rename "Methods" → "Data and Methods"
4. Convert footnotes to endnotes in a "NOTES" section
5. Ensure abstract < 150 words with sample size, design, and data source
6. Add title page with word count, table count, figure count, running head
7. Remove all author-identifying information
8. Stay under 10,000 words total

## File Structure

```
jhsb_submission/
├── CLAUDE.md                   # This file
├── partisan_differences.qmd    # Main document
├── american-sociological-association.csl  # ASA citation style (needed)
├── apa.csl                     # Remove or replace with ASA
├── references.bib              # Bibliography
└── sections/
    ├── introduction.qmd        # Rename heading to none or "Background"
    ├── methods.qmd             # Rename to "Data and Methods"
    ├── results.qmd
    ├── discussion.qmd
    ├── appendix.qmd
    ├── conclusion.qmd
    └── outtakes.qmd
```

## Rendering

```bash
quarto render paper/jhsb_submission/partisan_differences.qmd --to pdf
```

Note: paths to data/output files (e.g., `../out/tables/`) will need to be updated to `../../out/tables/` since this is one directory deeper than the original paper.
