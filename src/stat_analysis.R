library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)


# Load the ABM simulation functions
source("src/simulation.R")

# Set directories for output folder
data_dir <- "data"
out <- file.path(data_dir, "outputs")
dir.create(out, showWarnings = FALSE)

# Simulation parameters ------------------

n_agents_levels <- 1500
minority_levels <- seq(0.15, 0.35, by = 0.05)
prej_discount_levels <- seq(0, 1, by = 0.1)

max_ticks <- 5000
n_repetitions <- 100  # Change to 100 after testing
adopt_thresh <- 0.50


# Run ABM parameter sweep ------------------

results <- run_sweep(
  n_agents_levels = n_agents_levels,
  minority_levels = minority_levels,
  prej_discount_levels = prej_discount_levels,
  n_repetitions = n_repetitions,
  adoption_threshold = adopt_thresh,
  max_ticks = max_ticks,
  base_seed = 123,
  record_series = FALSE,
  output_dir = data_dir
)


# Prepare simulation data for statistical analysis

dat <- results$runs %>%
  rename(
    adoption_prop = final_adoption,
    adoption_reached = threshold_attained,
    rounds_to_adoption = first_crossing_round,
    adoption_time = observation_time,
    final_support_prop = final_total_support
  )


# Check the simulation output

head(dat)

summary(dat$adoption_prop)

table(dat$adoption_reached)

# Check majority adoption threshold attainment

n_runs <- nrow(dat)
n_reached <- sum(dat$adoption_reached == 1)
n_not_reached <- sum(dat$adoption_reached == 0)

cat("\nThreshold attainment check\n")
cat("Total simulation runs:", n_runs, "\n")
cat("Threshold attained:", n_reached, "\n")
cat("Threshold not attained:", n_not_reached, "\n")
cat("Threshold attainment rate:", n_reached / n_runs, "\n")

# Check threshold attainment for each parameter combination
threshold_summary <- dat %>%
  group_by(minority_prop, prej_discount) %>%
  summarise(
    n_runs = n(),
    n_reached = sum(adoption_reached == 1),
    attainment_rate = mean(adoption_reached == 1),
    .groups = "drop"
  )

print(threshold_summary, n = Inf)

write.csv(
  threshold_summary,
  file.path(data_dir, "threshold_by_condition.csv"),
  row.names = FALSE
)

if (n_reached == 0) {
  cat(
    "\nNo simulation reached the majority adoption threshold.",
    "\nBinary logistic regression and time-to-threshold analyses",
    "will not be conducted.\n"
  )
}

# ============================================================
# Descriptive analysis
# ============================================================

summary_data <- dat %>%
  group_by(minority_prop, prej_discount) %>%
  summarise(
    mean_adoption_prop = mean(adoption_prop),
    sd_adoption_prop = sd(adoption_prop),
    p_adoption = mean(adoption_reached),
    mean_rounds_to_adoption = if (any(adoption_reached == 1)) {
      mean(rounds_to_adoption[adoption_reached == 1])
    } else {
      NA_real_
    },
    .groups = "drop"
  )

print(summary_data)

write.csv(
  summary_data,
  file.path(data_dir, "summary_for_analysis.csv"),
  row.names = FALSE
)


# Majority adoption analysis

# Center IV
dat <- dat %>%
  mutate(
    minority_c = minority_prop - 0.25,
    prej_discount_c = prej_discount - 0.50
  )


# ============================================================
# Hypotheses testing
# ============================================================


# H1: Higher minority proportion increases majority adoption proportion.
# H2: Higher prejudice discounting decreases majority adoption proportion.
# H3: Prejudice discounting moderates the effect of minority proportion
#     on majority adoption proportion.

# ============================================================
# H1-H3: Fractional logistic regression
# ============================================================

# Outcome: Final proportion of original majority agents
# holding only the pro-immigrant attitude (adoption_prop).
#
# We initially considered ordinary linear regression.
# However, adoption_prop is bounded between 0 and 1,
# and the linear model produced predictions outside this range:
# 170 out of 550 fitted values (30.9%).
#
# We therefore use fractional logistic regression with a
# quasibinomial family and logit link.
#
# This approach:
# - accommodates proportions including exact 0 and 1;
# - constrains fitted values to the interval [0, 1];
# - allows testing the effects of minority proportion,
#   prejudice discounting, and their interaction.
#
# Coefficients are interpreted on the logit scale.
# Predicted proportions are used to interpret substantive effects.


# Model 0: Main effects
model_adoption_prop_0 <- glm(
  adoption_prop ~ minority_c + prej_discount_c,
  data = dat,
  family = quasibinomial(link = "logit")
)

summary(model_adoption_prop_0)


# Model 1: Main effects and interaction
model_adoption_prop_1 <- glm(
  adoption_prop ~ minority_c * prej_discount_c,
  data = dat,
  family = quasibinomial(link = "logit")
)

summary(model_adoption_prop_1)


# Compare models using an F-test
# Quasibinomial models use quasi-likelihood rather than
# a full likelihood, so a standard likelihood-ratio test
# or AIC comparison is not appropriate.

anova(
  model_adoption_prop_0,
  model_adoption_prop_1,
  test = "F"
)

# Fractional logistic regression diagnostics ------------------

# Check fitted values remain within [0, 1]
predicted_values <- fitted(model_adoption_prop_1)

n_outside <- sum(predicted_values < 0 | predicted_values > 1)
prop_outside <- mean(predicted_values < 0 | predicted_values > 1)

cat("Fitted value range:\n")
print(range(predicted_values))

cat("Number of fitted values outside [0, 1]:", n_outside, "\n")
cat("Proportion outside [0, 1]:", prop_outside, "\n")

# Pearson residuals vs fitted values
png(
  file.path(out, "fractional_logit_residuals.png"),
  width = 900,
  height = 650
)

plot(
  fitted(model_adoption_prop_1),
  residuals(model_adoption_prop_1, type = "pearson"),
  xlab = "Predicted majority adoption proportion",
  ylab = "Pearson residuals",
  main = "Fractional Logit: Residuals vs Fitted"
)

abline(h = 0, lty = 2)

dev.off()

# H3: Moderation analysis ------------------

# Test interaction on the logit scale
interaction_p <- summary(model_adoption_prop_1)$coefficients[
  "minority_c:prej_discount_c", "Pr(>|t|)"
]

# Predict majority adoption proportion on the response scale
prediction_data <- expand.grid(
  minority_prop = minority_levels,
  prej_discount = prej_discount_levels
) %>%
  mutate(
    minority_c = minority_prop - 0.25,
    prej_discount_c = prej_discount - 0.50
  )

prediction_data$predicted_adoption <- predict(
  model_adoption_prop_1,
  newdata = prediction_data,
  type = "response"
)

# Visualize the interaction at selected prejudice levels
plot_H3 <- prediction_data %>%
  filter(prej_discount %in% c(0, 0.5, 1)) %>%
  ggplot(aes(
    x = minority_prop,
    y = predicted_adoption,
    color = factor(prej_discount),
    group = factor(prej_discount)
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Minority proportion × Prejudice discounting",
    x = "Committed minority proportion",
    y = "Predicted majority adoption proportion",
    color = "Prejudice discounting"
  ) +
  theme_minimal()

print(plot_H3)

ggsave(
  file.path(out, "interaction_predicted_adoption.png"),
  plot_H3,
  width = 8,
  height = 6,
  dpi = 300
)


# Logistic regression: Majority adoption threshold attainment
# Outcome: Whether more than 50% of the original majority
# ever adopts the pro-immigrant attitude during a simulation.
# 0 = threshold not attained; 1 = threshold attained.
# Model 0: Main effects; Model 1: Interaction.


model_adoption_0 <- glm(
  adoption_reached ~ minority_c + prej_discount_c,
  data = dat,
  family = binomial(),
  control = glm.control(maxit = 100)
)

model_adoption_1 <- glm(
  adoption_reached ~ minority_c * prej_discount_c,
  data = dat,
  family = binomial(),
  control = glm.control(maxit = 100)
)

summary(model_adoption_0)
summary(model_adoption_1)

anova(model_adoption_0, model_adoption_1, test = "Chisq")

# Check binary logistic regression convergence
cat("Logistic model converged:", model_adoption_1$converged, "\n")

cat("Range of fitted probabilities:\n")
print(range(fitted(model_adoption_1)))

# Unpack logistic regression interaction ------------------

# Predict probability of majority adoption threshold attainment
prediction_logistic <- expand.grid(
  minority_prop = minority_levels,
  prej_discount = prej_discount_levels
) %>%
  mutate(
    minority_c = minority_prop - 0.25,
    prej_discount_c = prej_discount - 0.50
  )

prediction_logistic$predicted_probability <- predict(
  model_adoption_1,
  newdata = prediction_logistic,
  type = "response"
)

# Plot predicted probabilities at selected prejudice levels
plot_logistic <- prediction_logistic %>%
  filter(prej_discount %in% c(0, 0.5, 1)) %>%
  ggplot(aes(
    x = minority_prop,
    y = predicted_probability,
    color = factor(prej_discount),
    group = factor(prej_discount)
  )) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Majority adoption threshold attainment",
    x = "Committed minority proportion",
    y = "Predicted probability of threshold attainment",
    color = "Prejudice discounting"
  ) +
  theme_minimal()

print(plot_logistic)

ggsave(
  file.path(out, "logistic_interaction.png"),
  plot_logistic,
  width = 8,
  height = 6,
  dpi = 300
)

# Cox regression, survival analysis
# Test how predictors affect time to majority adoption threshold attainment.
# Runs without threshold attainment are right-censored at max_ticks.
# Time is measured in conversation ticks, not population-wide rounds.
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

# Time to majority adoption threshold attainment

model_cox_adoption_0 <- fit_cox("adoption_time", "adoption_reached", FALSE)

model_cox_adoption_1 <- fit_cox("adoption_time", "adoption_reached", TRUE)

# Display model results.
for (model in list(
  model_cox_adoption_0,
  model_cox_adoption_1
)) {
  if (!is.null(model)) print(summary(model))
}



# Kaplan-Meier curves
# Kaplan-Meier: Compare time to majority adoption threshold attainment
# across low, medium, and high prejudice discounting.
# Group prejudice discounting into three levels.
dat$prej_discount_group <- cut(
  dat$prej_discount,
  breaks = c(0, 0.3, 0.7, 1),
  labels = c("Low", "Medium", "High"),
  include.lowest = TRUE
)
table(dat$prej_discount, dat$prej_discount_group)


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
    xlab = "Conversation ticks (one speaker-hearer interaction per tick)",
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
  "adoption_time",
  "adoption_reached",
  "KM_adoption.png",
  "Time to majority adoption threshold attainment"
)


# ============================================================
# Heatmaps
# ============================================================

# Visualize majority adoption proportion and threshold attainment
# probability across parameter combinations.
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
  "p_adoption",
  "Probability of >50% adoption among original majority agents",
  "adoption_probability_heatmap.png"
)

plot_heat(
  "mean_adoption_prop",
  "Mean adoption proportion among original majority agents",
  "adoption_proportion_heatmap.png"
)

# ============================================================
# Export regression results
# ============================================================
# Export Model 0, Model 1, and model comparisons


sink(file.path(out, "model_results.txt"))

cat("ABM simulation results\n\n")

cat("Majority adoption proportion: Model 0\n")
print(summary(model_adoption_prop_0))

cat("\nMajority adoption proportion: Model 1\n")
print(summary(model_adoption_prop_1))

cat("\nFractional logistic regression model comparison\n")
print(anova(
  model_adoption_prop_0,
  model_adoption_prop_1,
  test = "F"
))

cat("\nModeration analysis\n")
cat("Interaction p-value:", interaction_p, "\n")

cat("\nPredicted majority adoption proportions\n")
print(prediction_data)

cat("\nMajority adoption regression diagnostics\n")

cat("Fitted value range:\n")
print(range(fitted(model_adoption_prop_1)))

cat("\nNumber of fitted values outside [0, 1]:\n")
print(n_outside)

cat("\nProportion of fitted values outside [0, 1]:\n")
print(prop_outside)


cat("\nThreshold attainment: Model 0\n")
print(summary(model_adoption_0))

cat("\nThreshold attainment: Model 1\n")
print(summary(model_adoption_1))

cat("\nLogistic model comparison\n")
print(anova(model_adoption_0, model_adoption_1, test = "Chisq"))

cat("\nLogistic regression interaction: Predicted probabilities\n")
print(prediction_logistic)

cat("\nCox adoption models\n")
if (!is.null(model_cox_adoption_0))
  print(summary(model_cox_adoption_0))
if (!is.null(model_cox_adoption_1))
  print(summary(model_cox_adoption_1))

sink()

cat("Analysis completed.\n")
cat("Results saved in:", out, "\n")