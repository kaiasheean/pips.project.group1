
validate_results <- function(data) {

  # 1. Check for missing adoption values
  stopifnot(all(!is.na(data$final_adoption)))

  # 2. Check adoption is between 0 and 1
  stopifnot(all(data$final_adoption >= 0))
  stopifnot(all(data$final_adoption <= 1))

  # 3. Check that majority agent counts add up
  total <- data$final_anti_majority_n +
    data$final_pro_majority_n +
    data$final_mixed_majority_n

  stopifnot(all(total == data$n_majority))

  # 4. Check threshold attainment values
  stopifnot(all(data$threshold_attained %in% c(0, 1)))

  # 5. Check that adoption matches the pro-majority count
  stopifnot(
    all(
      abs(data$final_adoption -
            data$final_pro_majority_n / data$n_majority) < 1e-10
    )
  )

  message("All validation checks passed!")
}
