# Attitude Change in Minority–Majority Interactions

## Project overview

This project was developed for the Programming in Psychological Science course at the University of Amsterdam.

We developed an agent-based model (ABM) to investigate how the proportion of a committed minority and prejudice discounting influence pro-immigration attitude adoption among majority agents.

The model simulates interactions between committed minority agents and non-committed majority agents. Prejudice discounting reduces the probability that agents respond to influence from committed minority speakers.

These are simplifying modelling assumptions and do not represent the full complexity of real-world immigration attitudes.

## Research question

How do committed minority proportion and prejudice discounting affect the adoption of pro-immigration attitudes among majority agents?

## Project structure
-  README.md — Project overview, dependencies, and reproduction instructions.
- `main.Rmd` — Final report, including the model description, validation, statistical analysis, results, and discussion.
- `src/simulation.R` — Agent-based simulation functions and Shiny application code.
- `src/stat_analysis.R` — Statistical analyses and visualizations.
- `src/validation.R` — Data validation and consistency checks.
- `data/simulation_runs.csv` — Saved simulation results used in the final analysis.
- `data/outputs/` — Additional simulation outputs, figures, and statistical results.
- `references.bib` — Bibliographic references.
- `apa.csl` — APA citation formatting.

## Experimental design

The final experiment used:

| Parameter | Value |
|---|---|
| Population size | 1,500 agents |
| Committed minority proportion | 0.15–0.35, increments of 0.05 |
| Prejudice discounting | 0–1, increments of 0.1 |
| Maximum simulation duration | 5,000 conversation ticks |
| Repetitions per condition | 100 |
| Parameter combinations | 55 |
| Total simulation runs | 5,500 |
| Adoption threshold | More than 50% of majority agents |

One tick represents one conversation between two agents.

## How to reproduce the report

1. Download or clone this repository.
2. Open the repository in RStudio and set the repository root as the working directory.
3. Install the required R packages:

   ```r
   install.packages(c(
     "shiny", "bslib", "dplyr", "ggplot2",
     "tidyr", "survival", "knitr", "rmarkdown"
   ))
   ```

4. Ensure that `data/simulation_runs.csv` is available.
5. Open `main.Rmd` and click **Knit** to generate the HTML report.

The report loads and validates the saved simulation dataset, runs a small reproducible simulation demonstration, performs the statistical analysis, and generates the visualizations.

The full dataset contains 5,500 simulation runs. These are loaded from `data/simulation_runs.csv` rather than regenerated during knitting.

The statistical analysis can also be reproduced separately by running `src/stat_analysis.R` from the repository root. This script uses the saved simulation results rather than rerunning the complete parameter sweep.

### Reproducibility

The complete simulation dataset is included in `data/simulation_runs.csv`.

The full experiment consists of 5,500 simulation runs, each containing 1,500 agents and up to 5,000 conversation ticks.

To make the report reproducible without requiring users to regenerate all 5,500 runs, `main.Rmd` loads the saved dataset.

A smaller demonstration using 100 agents and two repetitions is executed when knitting to demonstrate that the simulation code runs successfully.

The demonstration uses a fixed random seed and saves its outputs to a temporary directory, keeping the final dataset unchanged.

### Supporting scripts

- `src/simulation.R` contains the agent-based simulation functions.
- `src/validation.R` contains the data validation checks.
- `src/stat_analysis.R` contains additional statistical analyses and visualizations.

### Important limitations

This is a theoretical agent-based model rather than an empirical study of immigration attitudes.

The committed minority and prejudice-discounting mechanisms are simplifying assumptions. Results describe the model's behavior under the specified rules and parameter settings and should not be interpreted as direct evidence about real-world psychological processes.
