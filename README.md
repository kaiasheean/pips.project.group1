# Attitude Change in Minority–Majority Interactions

## Project overview

This project was developed for the Programming in Psychological Science course at the University of Amsterdam.

We developed an agent-based model (ABM) to investigate how the proportion of a committed minority and prejudice discounting influence pro-immigration attitude adoption among majority agents.

The model simulates interactions between committed minority agents and non-committed majority agents. Prejudice discounting reduces the probability that agents respond to influence from committed minority speakers.

These are simplifying modelling assumptions and do not represent the full complexity of real-world immigration attitudes.

## Research question

How do committed minority proportion and prejudice discounting affect the adoption of pro-immigration attitudes among majority agents?

## Project structure

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
2. Open the project in RStudio.
3. Ensure the