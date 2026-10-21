library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)

set.seed(123)

out <- "outputs"
dir.create(out, showWarnings = FALSE)


# Simulation parameters ------------------

n_agents_levels <- c(100, 200, 500)
minority_levels <- seq(0.15, 0.35, by = 0.05)
prej_discount_levels <- seq(0, 1, by = 0.1)
n_rounds <- 120
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
    
    influence <-
      (group_n / n_agents) * (1 - prej_discount) +
      1.7 * peer_fraction
    
    p_switch <- min(
      0.50,
      max(0, 0.006 + 0.085 * influence)
    )
    
    n_new <- rbinom(
      1,
      size = majority_n - adopted,
      prob = p_switch
    )
    
    adopted <- adopted + n_new
    
    support[round + 1] <-
      (group_n + adopted) / n_agents
  }
  
  # Detect tipping-like acceleration
  delta <- diff(support)
  tipping_round <- NA_integer_
  
  if (length(delta) >= 2 * TIP_WINDOW) {
    
    for (t in seq.int(2 * TIP_WINDOW, length(delta))) {
      
      before <- mean(
        delta[(t - 2 * TIP_WINDOW + 1):(t - TIP_WINDOW)]
      )
      
      after <- mean(
        delta[(t - TIP_WINDOW + 1):t]
      )
      
      if (
        after >= TIP_MULTIPLIER * max(before, 0.0005) &&
        after - before >= TIP_MIN_INCREASE
      ) {
        tipping_round <- t
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
    tipping_reached = as.integer(!is.na(tipping_round)),
    rounds_to_tipping = tipping_round,
    adoption_reached = as.integer(!is.na(adoption_round)),
    rounds_to_adoption = adoption_round,
    tipping_time = ifelse(
      is.na(tipping_round), n_round, tipping_round
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
  file.path(out, "simulation_timeseries.csv"),
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
  file.path(out, "simulation_runs.csv"),
  row.names = FALSE
)

# ============================================================
# Descriptive analysis
# ============================================================

summary_data <- dat %>%
  group_by(minority_proportion, prej_discount) %>%
  summarise(
    mean_attitude_change = mean(attitude_change_prop),
    sd_attitude_change = sd(attitude_change_prop),
    mean_final_support = mean(final_support_prop),
    p_tipping = mean(tipping_reached),
    p_adoption = mean(adoption_reached),
    .groups = "drop"
  )

print(summary_data)

write.csv(
  summary_data,
  file.path(out, "summary_by_parameter.csv"),
  row.names = FALSE
)


#Attitude change

# Center IV
dat <- dat %>%
  mutate(
    minority_c = minority_prop - 0.25,
    prej_discount_c = prej_discount - 0.50
  )


#H1: Minority proportion increases attitude change.
#H2: Prejudice discounting decreases attitude change.
#H3: Prejudice discounting weakens the minority proportion effect.

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