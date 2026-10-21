library(dplyr)
library(ggplot2)
library(tidyr)
library(survival)

set.seed(123)

# create a folder "output" to store results, figures, csv
out <- "outputs"
dir.create(out, showWarnings = FALSE)


# Simulation parameters ------------------

n_agents_levels <- c(100, 200, 500)
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
  tipping_round <- NA_integer_
  
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
  group_by(minority_prop, prej_discount) %>%
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


# Main effects - minority proportion and prejudice discounting
# H1: A higher committed minority proportion increases attitude change among majority agents.
# H2: Higher prejudice discounting decreases attitude change among majority agents.
# H3: Prejudice discounting weakens the positive relationship between minority proportion and attitude change.

# Model 0: Main effects only, no interaction term
model_attitude_0 <- lm(
  attitude_change_prop ~
    minority_c +
    prej_discount_c +
    factor(n_agents),
  data = dat
)

summary(model_attitude_0)


# Model 1: with interaction term
model_attitude_1 <- lm(
  attitude_change_prop ~
    minority_c * prej_discount_c +
    factor(n_agents),
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