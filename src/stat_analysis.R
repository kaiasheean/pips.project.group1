library(dplyr)
library(ggplot2)
library(tidyr)

set.seed(123)

# ============================================================
# 1. Generate synthetic data
# ============================================================

N <- 500

minority_levels <- seq(0.15, 0.35, by = 0.02)
prejudice_levels <- seq(0, 100, by = 10)

n_repetitions <- 100

data <- expand.grid(
  minority_proportion = minority_levels,
  prejudice = prejudice_levels,
  repetition = seq_len(n_repetitions)
)

# Population composition
data$n_minority <- round(N * data$minority_proportion)
data$n_majority <- N - data$n_minority

# Center predictors
data$minority_c <-
  (data$minority_proportion - 0.25) / 0.10

data$prejudice_c <-
  (data$prejudice - 50) / 50

# ============================================================
# 2. Generate switching probabilities
# ============================================================

# Illustrative assumptions, not estimated parameters.
# Higher minority proportion increases switching probability.
# Higher prejudice decreases switching probability.
# The interaction allows prejudice to weaken the minority effect.

linear_predictor <- -0.2 +
  1.5 * data$minority_c -
  2.0 * data$prejudice_c -
  0.4 * data$minority_c * data$prejudice_c

data$p_switch <- plogis(linear_predictor)

# Generate switching outcomes
data$n_switched <- rbinom(
  n = nrow(data),
  size = data$n_majority,
  prob = data$p_switch
)

data$n_not_switched <-
  data$n_majority - data$n_switched

# Primary outcome
data$attitude_change_proportion <-
  data$n_switched / data$n_majority

# Secondary descriptive outcome
data$final_support_rate <-
  (data$n_minority + data$n_switched) / N

# Export synthetic data
write.csv(
  data,
  "synthetic_simulation_data.csv",
  row.names = FALSE
)

# ============================================================
# 3. Descriptive analysis
# ============================================================

summary_data <- data %>%
  group_by(minority_proportion, prejudice) %>%
  summarise(
    mean_attitude_change =
      mean(attitude_change_proportion),
    sd_attitude_change =
      sd(attitude_change_proportion),
    mean_final_support =
      mean(final_support_rate),
    .groups = "drop"
  )

print(summary_data)

write.csv(
  summary_data,
  "summary_by_parameter.csv",
  row.names = FALSE
)

# ============================================================
# 4. H1: Effect of minority proportion
# ============================================================

# Binomial regression:
# number switched vs. number not switched.
# Adjust for prejudice.

model_H1 <- glm(
  cbind(n_switched, n_not_switched) ~
    minority_c + prejudice_c,
  family = binomial(link = "logit"),
  data = data
)

summary(model_H1)

# Visualise attitude change by minority proportion
plot_H1 <- ggplot(
  summary_data,
  aes(
    x = minority_proportion,
    y = mean_attitude_change
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ prejudice) +
  labs(
    title = "Attitude change by minority proportion",
    x = "Initial minority proportion",
    y = "Mean attitude change proportion"
  ) +
  theme_minimal()

print(plot_H1)

ggsave(
  "H1_minority_proportion.png",
  plot_H1,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 5. H2: Effect of prejudice
# ============================================================

# Adjust for minority proportion.

model_H2 <- glm(
  cbind(n_switched, n_not_switched) ~
    prejudice_c + minority_c,
  family = binomial(link = "logit"),
  data = data
)

summary(model_H2)

plot_H2 <- ggplot(
  summary_data,
  aes(
    x = prejudice,
    y = mean_attitude_change
  )
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ minority_proportion) +
  labs(
    title = "Attitude change by prejudice level",
    x = "Initial prejudice level",
    y = "Mean attitude change proportion"
  ) +
  theme_minimal()

print(plot_H2)

ggsave(
  "H2_prejudice.png",
  plot_H2,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 6. Interaction: Does prejudice weaken the minority effect?
# ============================================================

model_interaction <- glm(
  cbind(n_switched, n_not_switched) ~
    minority_c * prejudice_c,
  family = binomial(link = "logit"),
  data = data
)

summary(model_interaction)

# Compare additive and interaction models
model_additive <- glm(
  cbind(n_switched, n_not_switched) ~
    minority_c + prejudice_c,
  family = binomial(link = "logit"),
  data = data
)

anova(
  model_additive,
  model_interaction,
  test = "Chisq"
)

# Export regression coefficients
extract_coefficients <- function(model, model_name) {
  result <- as.data.frame(coef(summary(model)))
  result$term <- rownames(result)
  rownames(result) <- NULL
  result$model <- model_name
  result
}

regression_results <- bind_rows(
  extract_coefficients(model_H1, "H1"),
  extract_coefficients(model_H2, "H2"),
  extract_coefficients(model_interaction, "Interaction")
)

write.csv(
  regression_results,
  "regression_results.csv",
  row.names = FALSE
)

cat("Analysis completed.\n")
cat("Synthetic data, regression results, and plots saved.\n")