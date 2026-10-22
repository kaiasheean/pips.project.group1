library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)

set.seed(123)

# # Set directories for output folder
data_dir <- "data"
out <- file.path(data_dir, "outputs")
dir.create(out, showWarnings = FALSE)


# Simulation parameters ------------------

n_agents_levels <- 1500
minority_levels <- seq(0.15, 0.35, by = 0.05)
prej_discount_levels <- seq(0, 1, by = 0.1)
n_round <- 120
n_repetitions <- 10 # Change to 100 after testing

adopt_thresh <- 0.50

# Exploratory tipping detection criteria
TIP_WINDOW <- 8
TIP_MULTIPLIER <- 1.7
TIP_MIN_INCREASE <- 0.0015

# Parameter sweep -------------------------
conditions <- expand.grid(
  n_agents = n_agents_levels,
  minority_prop = minority_levels,
  prej_discount = prej_discount_levels,
  replicate = seq_len(n_repetitions),
  KEEP.OUT.ATTRS = FALSE
)

conditions$run_id <- seq_len(nrow(conditions))


# Simulate one run --------------------------

simulate_one <- function(n_agents, minority_prop,
                         prej_discount, replicate, run_id) {
  
  group_n <- round(n_agents * minority_prop)
  majority_n <- n_agents - group_n
  
  adopted <- 0L
  support <- numeric(n_round + 1)
  support[1] <- group_n / n_agents
  
  # Simulate attitude adoption over rounds
  for (round in seq_len(n_round)) {
    
    peer_fraction <- adopted / n_agents
    
    influence <- (group_n / n_agents) * (1 - prej_discount) + 1.7 * peer_fraction
    
    p_switch <- min(0.50,max(0, 0.006 + 0.085 * influence))
    
    n_new <- rbinom(1,
      size = majority_n - adopted,
      prob = p_switch
    )
    
    adopted <- adopted + n_new
    
    support[round + 1] <- (group_n + adopted) / n_agents
  }
  
  # Detect tipping-like acceleration
  delta <- diff(support)
  acceleration_round <- NA_integer_
  
  if (length(delta) >= 2 * TIP_WINDOW) {
    
    for (t in seq.int(2 * TIP_WINDOW, length(delta))) {
      
      before <- mean(
        delta[(t - 2 * TIP_WINDOW + 1):(t - TIP_WINDOW)]
      )
      
      after <- mean(delta[(t - TIP_WINDOW + 1):t])
      
      if (
        after >= TIP_MULTIPLIER * max(before, 0.0005) &&
        after - before >= TIP_MIN_INCREASE
      ) {
        acceleration_round <- t
        break
      }
    }
  }
  
  # Detect majority adoption
  adoption_hit <- which(support > adopt_thresh)
  
  adoption_round <- if (length(adoption_hit) > 0) {
    adoption_hit[1] - 1L
  } else {
    NA_integer_
  }
  
  # Run-level outcomes
  summary <- data.frame(
    run_id = run_id,
    replicate = replicate,
    n_agents = n_agents,
    minority_prop = minority_prop,
    prej_discount = prej_discount,
    group_n = group_n,
    majority_n = majority_n,
    changed_majority_n = adopted,
    attitude_change_prop = adopted / majority_n,
    final_support_prop = tail(support, 1),
    acceleration_reached = as.integer(!is.na(acceleration_round)),
    rounds_to_tipping = acceleration_round,
    adoption_reached = as.integer(!is.na(adoption_round)),
    rounds_to_adoption = adoption_round,
    tipping_time = ifelse(
      is.na(acceleration_round), n_round, acceleration_round
    ),
    adoption_time = ifelse(
      is.na(adoption_round), n_round, adoption_round
    )
  )
  
  # Round-level outcomes
  series <- data.frame(
    run_id = run_id,
    round = 0:n_round,
    support_prop = support
  )
  
  return(list(
    summary = summary,
    series = series
  ))
}

# Run simulations-------------------------

cat("Simulating", nrow(conditions), "runs...\n")

summary_list <- vector("list", nrow(conditions))

con <- file(
  file.path(data_dir, "simulation_timeseries.csv"),
  "wt"
)

for (i in seq_len(nrow(conditions))) {
  
  x <- conditions[i, ]
  
  z <- simulate_one(
    n_agents = x$n_agents,
    minority_prop = x$minority_prop,
    prej_discount = x$prej_discount,
    replicate = x$replicate,
    run_id = x$run_id
  )
  
  summary_list[[i]] <- z$summary
  
  write.table(
    z$series,
    con,
    sep = ",",
    row.names = FALSE,
    col.names = (i == 1),
    quote = FALSE
  )
}

close(con)

dat <- bind_rows(summary_list)

write.csv(
  dat,
  file.path(data_dir, "simulation_runs.csv"),
  row.names = FALSE
)

# ============================================================
# Descriptive analysis
# ============================================================

summary_data <- dat %>%
  group_by(minority_prop, prej_discount) %>%
  summarise(
    mean_attitude_change = mean(attitude_change_prop),
    sd_attitude_change = sd(attitude_change_prop),
    mean_final_support = mean(final_support_prop),
    p_tipping = mean(acceleration_reached),
    p_adoption = mean(adoption_reached),
    .groups = "drop"
  )

print(summary_data)

write.csv(
  summary_data,
  file.path(data_dir, "summary_by_parameter.csv"),
  row.names = FALSE
)


#Attitude change

# Center IV
dat <- dat %>%
  mutate(
    minority_c = minority_prop - 0.25,
    prej_discount_c = prej_discount - 0.50
  )


# ============================================================
# Hypotheses testing
# ============================================================


# Main effects - minority proportion and prejudice discounting
# Interaction - minority proportion * prejudice discounting
# H1: A higher committed minority proportion increases attitude change among majority agents.
# H2: Higher prejudice discounting decreases attitude change among majority agents.
# H3: Prejudice discounting weakens the positive relationship between minority proportion and attitude change.

# Model 0: Main effects only, no interaction term
model_attitude_0 <- lm(
  attitude_change_prop ~
    minority_c +
    prej_discount_c,
  data = dat
)

summary(model_attitude_0)


# Model 1: with interaction term
model_attitude_1 <- lm(
  attitude_change_prop ~
    minority_c * prej_discount_c,
  data = dat
)

summary(model_attitude_1)


# Compare models
anova(
  model_attitude_0,
  model_attitude_1
)
# H1 plot. Minority proportion
plot_H1 <- ggplot(
  summary_data,
  aes(
    x = minority_prop,
    y = mean_attitude_change
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ prej_discount) +
  labs(
    title = "Effect of minority proportion on attitude change",
    x = "Initial minority proportion",
    y = "Mean attitude change proportion"
  ) +
  theme_minimal()

print(plot_H1)

ggsave(
  file.path(out, "H1_minority_proportion.png"),
  plot_H1,
  width = 10,
  height = 7,
  dpi = 300
)


# H2 plot. Prejudice discounting

plot_H2 <- ggplot(
  summary_data,
  aes(x = prej_discount, y = mean_attitude_change)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ minority_prop) +
  labs(
    title = "Effect of prejudice discounting on attitude change",
    x = "Prejudice discounting",
    y = "Mean attitude change proportion"
  ) +
  theme_minimal()

print(plot_H2)

ggsave(
  file.path(out, "H2_prej_discount.png"),
  plot_H2,
  width = 10, height = 7, dpi = 300
)

# H3: Interaction plot

plot_H3 <- ggplot(
  summary_data,
  aes(
    x = minority_prop,
    y = mean_attitude_change,
    color = factor(prej_discount),
    group = prej_discount
  )
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Minority proportion × Prejudice discounting",
    x = "Initial minority proportion",
    y = "Mean attitude change proportion",
    color = "Prejudice discounting"
  ) +
  theme_minimal()

print(plot_H3)

ggsave(
  file.path(out, "H3_interaction.png"),
  plot_H3,
  width = 10, height = 7, dpi = 300
)



#H4 H5 Logistic regression
# H4: Higher minority proportion increases tipping probability, while prejudice discounting decreases it.

# H5: Higher minority proportion increases majority adoption probability, while prejudice discounting decreases it.
# Fit logistic regression for binary outcomes (0 = no, 1 = yes).
# Model 0: Main effects; Model 1: Interaction.

fit_logistic <- function(outcome, interaction = FALSE) {
  
  rhs <- if (interaction) {
    "minority_c * prej_discount_c"
  } else {
    "minority_c + prej_discount_c"
  }
  
  formula <- as.formula(paste(outcome, "~", rhs))
  
  # A logistic model cannot be estimated with one outcome class
  if (length(unique(dat[[outcome]])) < 2) {
    message(outcome, ": only one outcome class.")
    return(NULL)
  }
  
  model <- glm(
    formula,
    data = dat,
    family = binomial(),
    control = glm.control(maxit = 100)
  )
  
  if (!isTRUE(model$converged)) {
    warning(outcome, ": logistic model did not converge.")
  }
  
  return(model)
}


# H4: Tipping occurrence

model_tipping_0 <- fit_logistic(
  "acceleration_reached",
  interaction = FALSE
)

model_tipping_1 <- fit_logistic(
  "acceleration_reached",
  interaction = TRUE
)

# H5: Majority adoption occurrence

model_adoption_0 <- fit_logistic("adoption_reached", interaction = FALSE)

model_adoption_1 <- fit_logistic("adoption_reached", interaction = TRUE)

#function to display logistic regression results

show_logistic <- function(model) {
  if (is.null(model)) {
    cat("Model unavailable.\n")
  } else {
    print(summary(model))
    cat("Converged:", model$converged, "\n")
  }
}

# Compare Model 0 and Model 1 using a likelihood-ratio test.
compare_logistic <- function(model_0, model_1) {
  if (!is.null(model_0) &&
      !is.null(model_1) &&
      isTRUE(model_0$converged) &&
      isTRUE(model_1$converged)) {
    
    print(anova(model_0, model_1, test = "Chisq"))
    
  } else {
    cat("Comparison skipped: model unavailable or not converged.\n")
  }
}

show_logistic(model_tipping_0)
show_logistic(model_tipping_1)
compare_logistic(model_tipping_0, model_tipping_1)

show_logistic(model_adoption_0)
show_logistic(model_adoption_1)
compare_logistic(model_adoption_0, model_adoption_1)

# Cox regression, survival analysis
# H4-H5: Test how predictors affect time to tipping/adoption.
# Runs without an event are right-censored at n_round.
# Model 0: Main effects; Model 1: Interaction.

# Cox proportional hazards regression.
fit_cox <- function(time_var, event_var, interaction = FALSE) {
  
  # skip if no events occurred
  if (sum(dat[[event_var]]) == 0) {
    message(event_var, ": no events observed.")
    return(NULL)
  }
  
  rhs <- if (interaction) {
    "minority_c * prej_discount_c"
  } else {
    "minority_c + prej_discount_c"
  }
  
  formula <- as.formula(
    paste0(
      "Surv(", time_var, ", ", event_var, ") ~ ", rhs
    )
  )
  
  tryCatch(
    coxph(formula, data = dat),
    error = function(e) {
      message("Cox model error: ", conditionMessage(e))
      NULL
    }
  )
}

# Time to tipping (H4)

model_cox_tipping_0 <- fit_cox("tipping_time", "acceleration_reached", FALSE)

model_cox_tipping_1 <- fit_cox("tipping_time", "acceleration_reached", TRUE)

# Time to adoption (H5)

model_cox_adoption_0 <- fit_cox("adoption_time", "adoption_reached", FALSE)

model_cox_adoption_1 <- fit_cox("adoption_time", "adoption_reached", TRUE)

# Display model results.
for (model in list(
  model_cox_tipping_0, model_cox_tipping_1,
  model_cox_adoption_0, model_cox_adoption_1
)) {
  if (!is.null(model)) print(summary(model))
}



# Kaplan-Meier curves
# Kaplan-Meier: Compare time to tipping/adoption across low, medium, and high prejudice discounting.

# Group prejudice discounting into three levels.
dat$prej_discount_group <- cut(
  dat$prej_discount,
  breaks = c(-0.01, 0.3, 0.7, 1),
  labels = c("Low", "Medium", "High")
)


# Plot and save Kaplan-Meier survival curves.
plot_km <- function(time_var, event_var, filename, title) {
  
  formula <- as.formula(
    paste0(
      "Surv(", time_var, ", ", event_var,
      ") ~ prej_discount_group"
    )
  )
  
  fit <- survfit(formula, data = dat)
  
  png(file.path(out, filename), width = 1000, height = 700)
  
  plot(
    fit,
    col = 1:3,
    lwd = 2,
    xlab = "Round",
    ylab = "Probability of no event yet",
    main = title
  )
  
  legend(
    "topright",
    legend = c("Low", "Medium", "High"),
    col = 1:3,
    lty = 1,
    lwd = 2
  )
  
  dev.off()
}

plot_km(
  "tipping_time",
  "acceleration_reached",
  "KM_tipping.png",
  "Time to tipping-like acceleration"
)

plot_km(
  "adoption_time",
  "adoption_reached",
  "KM_adoption.png",
  "Time to majority adoption"
)


# ============================================================
# Heatmaps
# ============================================================

# Visualize attitude change, tipping probability, and adoption probability across parameter combinations.
# Create and save a heatmap for a selected outcome.

plot_heat <- function(variable, title, filename) {
  
  p <- ggplot(
    summary_data,
    aes(
      x = minority_prop,
      y = prej_discount,
      fill = .data[[variable]]
    )
  ) +
    geom_tile() +
    scale_fill_viridis_c(limits = c(0, 1)) +
    labs(
      title = title,
      x = "Committed minority proportion",
      y = "Prejudice discounting",
      fill = "Proportion"
    ) +
    theme_minimal()
  
  print(p)
  
  ggsave(
    file.path(out, filename),
    p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

plot_heat(
  "mean_attitude_change",
  "Mean attitude change",
  "attitude_change_heatmap.png"
)

plot_heat(
  "p_tipping",
  "Probability of tipping-like acceleration",
  "tipping_probability_heatmap.png"
)

plot_heat(
  "p_adoption",
  "Probability of majority adoption",
  "adoption_probability_heatmap.png"
)


# ============================================================
# Export regression results
# ============================================================
# Export Model 0, Model 1, and model comparisons


sink(file.path(out, "model_results.txt"))

cat("SYNTHETIC DATA - NOT FINAL ABM RESULTS\n\n")

cat("Attitude change: Model 0\n")
print(summary(model_attitude_0))

cat("\nAttitude change: Model 1\n")
print(summary(model_attitude_1))

cat("\nAttitude change model comparison\n")
print(anova(model_attitude_0, model_attitude_1))

cat("\nTipping occurrence: Model 0\n")
show_logistic(model_tipping_0)

cat("\nTipping occurrence: Model 1\n")
show_logistic(model_tipping_1)

cat("\nTipping model comparison\n")
compare_logistic(model_tipping_0, model_tipping_1)

cat("\nAdoption occurrence: Model 0\n")
show_logistic(model_adoption_0)

cat("\nAdoption occurrence: Model 1\n")
show_logistic(model_adoption_1)

cat("\nAdoption model comparison\n")
compare_logistic(model_adoption_0, model_adoption_1)

cat("\nCox tipping models\n")
if (!is.null(model_cox_tipping_0))
  print(summary(model_cox_tipping_0))
if (!is.null(model_cox_tipping_1))
  print(summary(model_cox_tipping_1))

cat("\nCox adoption models\n")
if (!is.null(model_cox_adoption_0))
  print(summary(model_cox_adoption_0))
if (!is.null(model_cox_adoption_1))
  print(summary(model_cox_adoption_1))

sink()

cat("Analysis completed.\n")
cat("Results saved in:", out, "\n")