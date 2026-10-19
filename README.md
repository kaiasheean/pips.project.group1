# Tipping points and prejudice: an agent-based model

A simulation app to explore how much an attitude shift in a population is
impaired by prejudice towards the committed minority advocating for change.
We investigate whether increasing the proportion of committed advocates 
produces a tipping point, a threshold beyond which population-level 
prejudice falls sharply.

Created for the Programming in Psychological Science course (UvA).


## Getting started
Clone the repository and open `main.Rmd` in RStudio:

    git clone https://github.com/kaiasheean/pips.project.group1.git

Install the required packages:

    install.packages(c("shiny", "ggplot2", "scales", "knitr", "rmarkdown", "here"))

Run the chunks of `main.Rmd` from top to bottom. The Shiny app chunk is not
evaluated when knitting and must be run manually.

## Repository structure
    data/        Simulation output (CSV)
    src/
      model.R    Agent-based model functions
      app.R      Shiny app
      analysis.R Statistical analysis and plots
    main.Rmd     Main deliverable
    README.md

## Dependencies
<!-- TODO: update this list once all code is merged -->

## References
See the bibliography in `main.Rmd`.
