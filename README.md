# Do Legislators Change Their Votes When They See District Polling?

A replication and matched-pair sensitivity analysis of **Butler & Nickerson (2011)**, *"Can Learning Constituency Opinion Affect How Legislators Vote? Results from a Field Experiment"* (*Quarterly Journal of Political Science* 6(1): 55–83).

- **Author:** Nolan Yumiaco (UC San Diego, POLI 100X: Data Analysis in Congress)
- **Live Report (GitHub Pages):** <https://nyum2003.github.io/butler-nickerson-replication/>
- **Original Study:** <https://doi.org/10.1561/100.00011019>
- **Yale ISPS Replication Archive:** <https://doi.org/10.60600/YU/QVCG0C>

## Key Finding

Replicating Butler & Nickerson (2011) with CR2 matched-pair clustered linear probability models yields an estimated treatment × low-support interaction (`FLT`) of **−29.3 percentage points** ($p = 0.053$, $N = 67$, $35$ pairs).

- **Leave-one-pair-out sensitivity:** `FLT` stays negative across all 35 single-pair exclusions, ranging from **−35.4 pp** to **−21.0 pp** (Pair `32` omitted).
- **Joint exclusion of Pairs 21 & 32:** `FLT` attenuates to **−16.1 pp** ($p = 0.202$, $N = 63$, $33$ pairs) in the controlled model while remaining **−36.3 pp** ($p = 0.041$) without controls.

## Repository Structure

```text
.
├── _quarto.yml                 # Quarto website configuration (outputs to docs/)
├── index.qmd                   # Main replication & sensitivity report
├── code.qmd                    # Web view of the reproducible R script
├── styles.css                  # Portfolio styling
├── Code/
│   ├── replication_analysis.R  # Clean, portable R script used by Quarto
│   └── Rep.Proj.1.1.R          # Original coursework script
├── Data/
│   └── butler_nickerson_clean.csv
├── assets/
│   └── POLI100X_Replication_Report.pdf
└── docs/                       # Rendered static HTML site for GitHub Pages
```

## Reproduce Locally

1. Install required R packages once:
   ```r
   install.packages(c("tidyverse", "estimatr", "scales", "knitr", "rmarkdown"))
   ```
2. Run the analysis script or render the Quarto site:
   ```bash
   Rscript Code/replication_analysis.R
   quarto render
   ```
