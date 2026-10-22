# Version_3.2_Simulation_app_data_preparation_for_analyses

# ================================================================
# USE AS ONE FILE OR SEPARATE FILES
# ================================================================
# ONE FILE: save this entire script as app.R and click Run App.
# To run the original checks before the app starts, uncomment the
# check_model() call at the end of Section 1.
# Alternatively, execute Section 1 only and call check_model() in RStudio.
# TWO FILES: save Section 1 as simulation.R and Section 2 as app.R.
# Keep both files in the same folder and launch app.R with Run App.
# OPTIONAL: later move individual functions into the files outlined below.
# All headings in this file are R comments and can be copied safely.
#
# CHANGED DEFAULT: run_sweep() uses n_agents_levels = 1500.
# Repetitions remain 10. run_model() and the Shiny controls are unchanged.
# The original check_model() is retained without additional tests.
#
# TWO ANALYSES:
# 1. Threshold-attainment frequency across repeated simulation runs.
# 2. Final pro-only majority adoption as a function of discounting.
#
# The second outcome is measured at max_ticks, excludes committed
# agents, and does NOT count mixed agents as fully pro-immigration.
# Crossing a chosen threshold is not an identified social tipping point.
# One tick = one conversation, not a population-wide round.
# This R/Shiny script has not been executed in the assistant environment.

# ================================================================
# SECTION 1 — SIMULATION.R
# ================================================================

# OPTIONAL TEMPLATE FOR SEPARATE FUNCTION FILES
# Create the R folder and move each COMPLETE function definition into
# its indicated file when ready. simulation.R can then be a loader.
# Keep simulation.R and app.R in the project folder, beside the R folder.
# Source paths below are relative to that project working directory.
#
# UNCOMMENT THESE LINES IN THE FINAL SEPARATE-FUNCTION-FILE PRODUCT.
# FILL IN THE PATHS WHEN READY; KEEP COMMENTED IN THE COMBINED SCRIPT.
# ONLY UNCOMMENT AFTER MOVING THE DEFINITIONS TO THOSE FILES.
# source("R/validators.R", local = TRUE)              # is_whole_number(), is_probability()
# source("R/run_model.R", local = TRUE)               # run_model()
# source("R/summarise_runs.R", local = TRUE)          # summarise_runs()
# source("R/prepare_analysis_tables.R", local = TRUE) # prepare_analysis_tables()
# source("R/run_sweep.R", local = TRUE)               # run_sweep()
# source("R/check_model.R", local = TRUE)             # Original check_model()
#
# Do not keep duplicate definitions in simulation.R after moving them.
# These source calls DEFINE functions; they do not execute simulations.

# Validate user/model settings before running the simulation.
is_whole_number <- function(x, minimum = 0) {
  is.numeric(x) && length(x) == 1 && is.finite(x) &&
    x >= minimum && x <= .Machine$integer.max && x == floor(x)
}
is_probability <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 0 && x <= 1
}

# MODEL: one population observed until max_ticks.
run_model <- function(n = 100, committed_share = 0.1,
                      adoption_threshold = 0.5, max_ticks = 5000,
                      prejudice_discount = 0,
                      run_id = 1, replicate = 1, batch_id = 1,
                      seed = NULL, record_series = FALSE,
                      record_every = 100) {
  stopifnot(is_whole_number(n, 2), is_whole_number(max_ticks, 1),
            is_whole_number(record_every, 1), is_probability(committed_share),
            is_probability(adoption_threshold), is_probability(prejudice_discount),
            is.logical(record_series), length(record_series) == 1,
            !is.na(record_series))
  n_committed <- round(n * committed_share)
  n_majority <- n - n_committed
  if (n_majority == 0) stop("At least one majority agent is required.")
  if (!is.null(seed)) {
    stopifnot(is_whole_number(seed))
    set.seed(seed)
  }
  
  # State codes: anti-only / pro-only / mixed.
  anti_img <- 0L
  pro_img <- 1L
  both <- 2L
  committed <- rep(FALSE, n)
  committed[sample.int(n, n_committed)] <- TRUE
  majority <- !committed
  attitude <- rep(anti_img, n)
  attitude[committed] <- pro_img
  first_crossing_round <- NA_integer_
  counts <- c(n_majority, 0, 0) # Majority anti / pro / mixed counts.
  
  # Optional trajectory storage. Initial/final ticks are always retained.
  series <- NULL
  if (record_series) {
    saved_ticks <- unique(c(seq(0, max_ticks, by = record_every), max_ticks))
    series <- data.frame(
      run_id = run_id, batch_id = batch_id, round = saved_ticks,
      majority_adoption_prop = NA_real_, majority_anti_prop = NA_real_,
      majority_mixed_prop = NA_real_, total_support_prop = NA_real_)
    state_columns <- c("majority_adoption_prop", "majority_anti_prop",
                       "majority_mixed_prop", "total_support_prop")
    series[1, state_columns] <- list(0, 1, 0, n_committed / n)
    next_save <- 2
  }
  
  # MODEL CORE: one conversation per iteration.
  for (conversation_round in seq_len(max_ticks)) {
    # Select a uniform speaker and uniformly chosen DIFFERENT listener.
    speaker <- sample.int(n, 1)
    hearer <- sample.int(n - 1, 1)
    if (hearer >= speaker) hearer <- hearer + 1
    expressed_attitude <- if (attitude[speaker] == both) {
      sample(c(anti_img, pro_img), 1)
    } else attitude[speaker]
    # Only influence from committed speakers is discounted.
    hearer_updates <- if (committed[speaker]) {
      runif(1) >= prejudice_discount
    } else TRUE
    
    # Original executable listener rules AND speaker collapse are preserved.
    if (attitude[hearer] == both || attitude[hearer] == expressed_attitude) {
      if (!committed[hearer] && hearer_updates) attitude[hearer] <- expressed_attitude
      if (!committed[speaker]) attitude[speaker] <- expressed_attitude
    } else {
      if (!committed[hearer] && hearer_updates) attitude[hearer] <- both
    }
    
    # Recount majority states. +1 translates codes 0/1/2 into bins 1/2/3.
    # Simpler than incremental bookkeeping but potentially slower for big sweeps.
    counts <- tabulate(attitude[majority] + 1L, nbins = 3)
    majority_adoption <- counts[2] / n_majority
    
    # ANALYSIS 1 MEASUREMENT: check every tick; keep the FIRST crossing.
    # Strict > matches Version 3.0. Do NOT stop when crossing happens.
    if (is.na(first_crossing_round) && majority_adoption > adoption_threshold) {
      first_crossing_round <- as.integer(conversation_round)
    }
    if (record_series && next_save <= nrow(series) &&
        conversation_round == series$round[next_save]) {
      series[next_save, state_columns] <- list(
        majority_adoption, counts[1] / n_majority, counts[3] / n_majority,
        (n_committed + counts[2]) / n)
      next_save <- next_save + 1
    }
  }
  
  # ANALYSIS 1 RUN OUTCOME: 1 if threshold was ever crossed, else 0.
  threshold_attained <- as.integer(!is.na(first_crossing_round))
  # ANALYSIS 2 RUN OUTCOME: majority pro-only proportion at max_ticks.
  # Includes ALL runs, not just runs that attained the threshold.
  final_adoption <- counts[2] / n_majority
  summary <- data.frame(
    run_id = run_id, batch_id = batch_id, replicate = replicate,
    seed = if (is.null(seed)) NA_real_ else seed,
    n_agents = n, minority_prop = committed_share,
    minority_prop_actual = n_committed / n, prej_discount = prejudice_discount,
    n_committed = n_committed, n_majority = n_majority,
    adoption_threshold = adoption_threshold, max_ticks = max_ticks,
    first_crossing_round = first_crossing_round,
    threshold_attained = threshold_attained,
    # Retained for survival analyses: event time or censoring time.
    observation_time = if (threshold_attained == 1) first_crossing_round else max_ticks,
    final_pro_majority_n = counts[2], final_adoption = final_adoption,
    final_anti_majority_n = counts[1], final_mixed_majority_n = counts[3],
    final_total_support = (n_committed + counts[2]) / n,
    # Ever crossed and above threshold at the end are different outcomes.
    final_above_threshold = as.integer(final_adoption > adoption_threshold))
  list(summary = summary, series = series)
}

# OUTPUT PROCESSING: one row per setting combination.
# NEW: number of attainers and standard error of mean final adoption.
# Different minority sizes/populations/durations are NEVER silently pooled.
summarise_runs <- function(dat) {
  if (is.null(dat) || nrow(dat) == 0) return(data.frame())
  keys <- c("n_agents", "minority_prop", "minority_prop_actual", "prej_discount",
            "adoption_threshold", "max_ticks")
  groups <- do.call(interaction, c(dat[keys], list(drop = TRUE, lex.order = TRUE)))
  pieces <- lapply(split(dat, groups), function(x) {
    crossed <- x$first_crossing_round[x$threshold_attained == 1]
    sd_final <- sd(x$final_adoption)
    cbind(x[1, keys, drop = FALSE], data.frame(
      n_runs = nrow(x),
      n_threshold_attained = sum(x$threshold_attained),
      p_threshold = mean(x$threshold_attained),
      mean_final_adoption = mean(x$final_adoption),
      sd_final_adoption = sd_final,
      # Between-RUN standard error, not an assumption of independent agents.
      # NA if there is only one run. Independence across runs is assumed.
      se_mean_final_adoption = sd_final / sqrt(nrow(x)),
      p_final_above_threshold = mean(x$final_above_threshold),
      median_first_crossing_among_attainers = if (length(crossed)) median(crossed) else NA_real_))
  })
  out <- do.call(rbind, pieces)
  # Sort by design settings so plotting/inspection is straightforward.
  out <- out[do.call(order, out[keys]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# NEW: two explicit analysis-ready tables from the same individual runs.
# Both retain minority size so a discounting comparison need not mix conditions.
prepare_analysis_tables <- function(dat) {
  s <- summarise_runs(dat)
  if (nrow(s) == 0) {
    return(list(threshold = data.frame(), majority_adoption = data.frame()))
  }
  settings <- c("n_agents", "minority_prop", "minority_prop_actual",
                "prej_discount", "adoption_threshold", "max_ticks")
  threshold <- s[c(settings, "n_runs", "n_threshold_attained", "p_threshold")]
  # Explicit percentage columns are for display; model inputs stay 0-1.
  threshold$threshold_attainment_percent <- 100 * threshold$p_threshold
  majority_adoption <- s[c(settings, "n_runs", "mean_final_adoption",
                           "sd_final_adoption", "se_mean_final_adoption")]
  majority_adoption$mean_final_adoption_percent <- 100 * majority_adoption$mean_final_adoption
  list(threshold = threshold, majority_adoption = majority_adoption)
}

# EXPERIMENT HELPER: automatically vary discounting and minority size.
# To study discounting at ONE minority size, set minority_levels = 0.15.
# CHANGED: default TOTAL POPULATION is 1500; repetitions remain 10.
run_sweep <- function(n_agents_levels = 100,
                      minority_levels = seq(0.15, 0.35, by = 0.05),
                      prej_discount_levels = seq(0, 1, by = 0.1),
                      n_repetitions = 10, adoption_threshold = 0.5,
                      max_ticks = 5000, base_seed = 123,
                      record_series = FALSE, record_every = 100,
                      output_dir = file.path("data", "outputs")) {
  stopifnot(is_whole_number(n_repetitions, 1), is_whole_number(base_seed),
            is.logical(record_series), length(record_series) == 1,
            !is.na(record_series))
  conditions <- expand.grid(
    n_agents = n_agents_levels, minority_prop = minority_levels,
    prej_discount = prej_discount_levels, replicate = seq_len(n_repetitions),
    KEEP.OUT.ATTRS = FALSE)
  if (!nrow(conditions)) stop("Supply at least one parameter combination.")
  stopifnot(as.double(base_seed) + nrow(conditions) - 1 <= .Machine$integer.max)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  summaries <- vector("list", nrow(conditions))
  if (record_series) {
    con <- file(file.path(output_dir, "simulation_timeseries.csv"), "wt")
    on.exit(close(con), add = TRUE)
  }
  for (i in seq_len(nrow(conditions))) {
    x <- conditions[i, ]
    z <- run_model(n = x$n_agents, committed_share = x$minority_prop,
                   prejudice_discount = x$prej_discount, adoption_threshold = adoption_threshold,
                   max_ticks = max_ticks, run_id = i, replicate = x$replicate,
                   seed = as.double(base_seed) + i - 1,
                   record_series = record_series, record_every = record_every)
    summaries[[i]] <- z$summary
    if (record_series) write.table(z$series, con, sep = ",", row.names = FALSE,
                                   col.names = (i == 1), quote = FALSE)
    if (i %% 25 == 0 || i == nrow(conditions)) message("Completed ", i, " / ", nrow(conditions))
  }
  dat <- do.call(rbind, summaries)
  summary_data <- summarise_runs(dat)
  analyses <- prepare_analysis_tables(dat)
  # Fixed filenames overwrite earlier exports: use a new output_dir to retain them.
  write.csv(dat, file.path(output_dir, "simulation_runs.csv"), row.names = FALSE)
  write.csv(summary_data, file.path(output_dir, "summary_by_parameter.csv"), row.names = FALSE)
  # NEW: two distinct files corresponding to the requested analyses.
  write.csv(analyses$threshold,
            file.path(output_dir, "analysis_1_threshold_attainment.csv"), row.names = FALSE)
  write.csv(analyses$majority_adoption,
            file.path(output_dir, "analysis_2_majority_adoption.csv"), row.names = FALSE)
  list(runs = dat, summary = summary_data,
       analysis_1 = analyses$threshold, analysis_2 = analyses$majority_adoption)
}

# Optional basic checks. These do not automatically run on opening the app.
# Original check_model() retained unchanged.
check_model <- function() {
  blocked <- run_model(prejudice_discount = 1, max_ticks = 100, seed = 123,
                       record_series = TRUE, record_every = 200)
  stopifnot(is.na(blocked$summary$first_crossing_round),
            blocked$summary$threshold_attained == 0, blocked$summary$final_adoption == 0,
            blocked$summary$observation_time == 100,
            identical(as.numeric(blocked$series$round), c(0, 100)))
  a <- run_model(committed_share = 0.35, max_ticks = 2000, seed = 456,
                 record_series = TRUE, record_every = 1)
  b <- run_model(committed_share = 0.35, max_ticks = 2000, seed = 456)
  cols <- c("majority_adoption_prop", "majority_anti_prop", "majority_mixed_prop")
  stopifnot(identical(a$summary, b$summary),
            all(abs(rowSums(a$series[cols]) - 1) < 1e-10),
            tail(a$series$majority_adoption_prop, 1) == a$summary$final_adoption)
  hits <- a$series$round[a$series$majority_adoption_prop > a$summary$adoption_threshold]
  expected <- if (length(hits)) as.integer(hits[1]) else NA_integer_
  stopifnot(identical(a$summary$first_crossing_round, expected))
  # NEW: check that the two analysis tables summarise the individual runs correctly.
  dat <- rbind(a$summary, b$summary)
  tables <- prepare_analysis_tables(dat)
  stopifnot(tables$threshold$n_runs == 2,
            tables$threshold$p_threshold == mean(dat$threshold_attained),
            tables$majority_adoption$mean_final_adoption == mean(dat$final_adoption))
  # a and b are duplicate seeded runs for testing only, NOT independent data.
  message("Basic consistency checks passed.")
}

# Example console sweep after sourcing simulation.R (not auto-executed):
# Uses the new default population of 1500; explicitly requests 50 repetitions.
# results <- run_sweep(minority_levels = 0.15,
#                      prej_discount_levels = seq(0, 1, by = 0.1),
#                      n_repetitions = 50,
#                      output_dir = file.path("data", "discounting_sweep_01"))
# results$analysis_1  # Frequencies of threshold attainment.
# results$analysis_2  # Final majority adoption at each discounting level.
# For graphs: x = prej_discount; y = p_threshold OR mean_final_adoption.
# For regression, use results$runs, not just the aggregated tables:
# glm(threshold_attained ~ prej_discount, family = binomial(), data = results$runs)
# lm(final_adoption ~ prej_discount, data = results$runs)
# These formulas assume minority size/population/duration held fixed.
# If minority size also varies, consider minority_prop * prej_discount.
# Check model assumptions/convergence; lm may not suit bounded outcomes.
# No statistical models or plots are executed automatically.

# OPTIONAL: UNCOMMENT TO RUN THE ORIGINAL CHECKS BEFORE THE APP STARTS.
# THIS IS OPTIONAL IN BOTH THE COMBINED AND FINAL SEPARATE-FILE PRODUCT.
# check_model()

# ===================== END OF SECTION 1 ==========================

# ================================================================
# SECTION 2 — APP.R
# ================================================================
library(shiny)
library(bslib)

# OPTIONAL FINAL TWO-FILE LOADER TEMPLATE:
# The automatic loader below already handles the two-file arrangement.
# If you prefer an explicit source() call, use this template instead.
# UNCOMMENT THIS LINE IN THE FINAL TWO-FILE PRODUCT IF REPLACING THE
# AUTOMATIC LOADER BELOW; FILL IN THE PATH WHEN READY.
# KEEP COMMENTED IN THE COMBINED SCRIPT.
# source("simulation.R", local = TRUE)
# Never source app.R from inside app.R.

# Combined file: Section 1 already defines these functions locally.
# Split files: load simulation.R if local model functions are absent.
# Remove the redundant loader check that was in the previous version.
.required_model_functions <- c("run_model", "summarise_runs",
                               "prepare_analysis_tables", "is_whole_number")
.model_is_loaded <- all(vapply(.required_model_functions, exists,
                               logical(1), envir = environment(), mode = "function", inherits = FALSE))
if (!.model_is_loaded) {
  if (!file.exists("simulation.R")) {
    stop(paste("Model functions are not loaded and simulation.R was not found.",
               "Run the entire combined script, or place simulation.R beside app.R",
               "and launch the app from that folder. Current working directory:", getwd()),
         call. = FALSE)
  }
  source("simulation.R", local = TRUE)
}
rm(.required_model_functions, .model_is_loaded)

# APP UI: controls and output placeholders, not model rules.
ui <- page_sidebar(
  title = "Threshold attainment and majority adoption",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  sidebar = sidebar(width = 340,
                    h5("Model settings"),
                    sliderInput("n", "Number of agents", min = 20, max = 1000, value = 100, step = 10),
                    sliderInput("share", "Committed group size (%)", min = 0, max = 50, value = 15, step = 1, post = "%"),
                    textOutput("committed_text"),
                    sliderInput("discount", "Prejudice discount rate", min = 0, max = 1, value = 0.25, step = 0.05),
                    helpText("0 = no discount. At 1, majority adoption cannot start from these initial states."),
                    sliderInput("threshold", "Adoption threshold (% of majority)", min = 10, max = 90, value = 50, step = 5, post = "%"),
                    helpText("Crossing means MORE THAN this share of majority agents is fully pro-immigration. Mixed and committed agents do not count."),
                    sliderInput("max_ticks", "Conversation ticks per run", min = 500, max = 50000, value = 5000, step = 500),
                    helpText("All runs continue to this endpoint, including after crossing."),
                    hr(), h5("Repetitions and recording"),
                    sliderInput("reps", "Repeated runs", min = 10, max = 200, value = 50, step = 10),
                    numericInput("base_seed", "Base random seed", value = 123, min = 0, max = 2000000000, step = 1),
                    checkboxInput("record_series", "Record time series", value = FALSE),
                    numericInput("record_every", "Save every this many ticks", value = 100, min = 1, step = 1),
                    helpText("Crossing is checked every tick regardless of recording frequency."),
                    actionButton("run", "Run simulations", class = "btn-primary w-100"),
                    hr(), h5("Downloads"),
                    helpText("To compare discounting: hold other settings fixed, change discounting and run again. Downloads contain ALL stored batches."),
                    helpText("Use non-overlapping seeds for additional repetitions of identical settings. Closing the session loses data unless downloaded."),
                    downloadButton("download_runs", "Individual run results"),
                    downloadButton("download_summary", "Full parameter summary"),
                    # NEW downloads correspond directly to the two requested analyses.
                    downloadButton("download_analysis_1", "Analysis 1: threshold frequency"),
                    downloadButton("download_analysis_2", "Analysis 2: majority adoption"),
                    downloadButton("download_series", "Recorded time series"),
                    actionButton("clear", "Clear stored batches")
  ),
  layout_columns(fill = FALSE, col_widths = c(4, 4, 4),
                 value_box(title = "1. Threshold attained (latest batch)", value = textOutput("threshold_prob"), theme = "primary"),
                 value_box(title = "2. Mean final majority adoption", value = textOutput("final_adoption"), theme = "secondary"),
                 value_box(title = "Median crossing tick (attainers only)", value = textOutput("median_rounds"), theme = "success")),
  card(card_header("Stored results"), textOutput("stored_status"),
       helpText("First crossing need not be sustained. Neither output identifies a social tipping point."), tableOutput("preview")),
  # NEW previews: both analyses, grouped by discounting AND other settings.
  card(card_header("Analysis 1: proportion of runs attaining the threshold"),
       tableOutput("analysis_1_preview")),
  card(card_header("Analysis 2: mean final pro-only majority adoption"),
       helpText("All runs are included, whether or not they attained the threshold."),
       tableOutput("analysis_2_preview"))
)

# APP SERVER: separate temporary storage per user session.
server <- function(input, output, session) {
  stored_runs <- reactiveVal(NULL)
  stored_series <- reactiveVal(NULL)
  latest_batch <- reactiveVal(NULL)
  batch_counter <- reactiveVal(0)
  output$committed_text <- renderText({
    paste0(round(input$share / 100 * input$n), " of ", input$n, " agents are committed")
  })
  
  # Run only on a button click; slider movements do not simulate automatically.
  observeEvent(input$run, {
    if (!is_whole_number(input$base_seed) ||
        as.double(input$base_seed) + input$reps - 1 > .Machine$integer.max) {
      showNotification("Enter a valid nonnegative integer seed.", type = "error")
      return(invisible(NULL))
    }
    if (!is_whole_number(input$record_every, 1)) {
      showNotification("Recording interval must be a positive integer.", type = "error")
      return(invisible(NULL))
    }
    batch <- batch_counter() + 1
    old <- stored_runs()
    first_id <- if (is.null(old)) 1 else max(old$run_id) + 1
    settings <- list(n = input$n, committed_share = input$share / 100,
                     prejudice_discount = input$discount, adoption_threshold = input$threshold / 100,
                     max_ticks = input$max_ticks, record_series = input$record_series,
                     record_every = input$record_every)
    reps <- input$reps
    base_seed <- input$base_seed
    # Reject duplicate settings/seed pairs already stored in this session.
    if (!is.null(old)) {
      same <- old$n_agents == settings$n & old$minority_prop == settings$committed_share &
        old$prej_discount == settings$prejudice_discount &
        old$adoption_threshold == settings$adoption_threshold & old$max_ticks == settings$max_ticks
      seeds <- base_seed + seq_len(reps) - 1
      if (any(same & old$seed %in% seeds)) {
        showNotification("Some settings/seed pairs are already stored. Use a non-overlapping seed range or clear results.", type = "error", duration = 10)
        return(invisible(NULL))
      }
    }
    # APP -> MODEL boundary: named arguments passed into the simulation.
    sims <- withProgress(message = "Running full-duration simulations...", value = 0, {
      lapply(seq_len(reps), function(i) {
        z <- do.call(run_model, c(settings, list(run_id = first_id + i - 1,
                                                 replicate = i, batch_id = batch, seed = base_seed + i - 1)))
        incProgress(1 / reps)
        z
      })
    })
    new_runs <- do.call(rbind, lapply(sims, function(z) z$summary))
    new_series <- do.call(rbind, lapply(sims, function(z) z$series))
    stored_runs(rbind(old, new_runs))
    if (!is.null(new_series)) stored_series(rbind(stored_series(), new_series))
    latest_batch(new_runs)
    batch_counter(batch)
  }, ignoreInit = TRUE)
  observeEvent(input$clear, {
    stored_runs(NULL); stored_series(NULL); latest_batch(NULL); batch_counter(0)
  })
  
  # NEW: calculate the two tables once whenever stored runs change.
  analysis_tables <- reactive({
    req(stored_runs())
    prepare_analysis_tables(stored_runs())
  })
  output$threshold_prob <- renderText({
    x <- latest_batch()
    if (is.null(x)) "Run simulations first" else sprintf("%.1f%%", 100 * mean(x$threshold_attained))
  })
  output$final_adoption <- renderText({
    x <- latest_batch()
    if (is.null(x)) "—" else sprintf("%.1f%%", 100 * mean(x$final_adoption))
  })
  output$median_rounds <- renderText({
    x <- latest_batch()
    if (is.null(x)) return("—")
    crossed <- x$first_crossing_round[x$threshold_attained == 1]
    if (!length(crossed)) "No crossings" else format(median(crossed), big.mark = ",", trim = TRUE)
  })
  output$stored_status <- renderText({
    x <- stored_runs()
    if (is.null(x)) return("No stored results yet.")
    paste(nrow(x), "runs in", length(unique(x$batch_id)), "batches.")
  })
  output$preview <- renderTable({
    req(latest_batch())
    head(latest_batch()[c("run_id", "minority_prop", "prej_discount", "first_crossing_round",
                          "threshold_attained", "final_adoption", "observation_time")], 10)
  }, digits = 3, na = "NA")
  output$analysis_1_preview <- renderTable({
    analysis_tables()$threshold
  }, digits = 3, na = "NA")
  output$analysis_2_preview <- renderTable({
    analysis_tables()$majority_adoption
  }, digits = 3, na = "NA")
  
  # Complete data exports; no statistical model fitting or graphs here.
  output$download_runs <- downloadHandler(filename = function() "simulation_runs.csv",
                                          content = function(file) {
                                            req(stored_runs()); write.csv(stored_runs(), file, row.names = FALSE, na = "NA")
                                          })
  output$download_summary <- downloadHandler(filename = function() "summary_by_parameter.csv",
                                             content = function(file) {
                                               req(stored_runs()); write.csv(summarise_runs(stored_runs()), file, row.names = FALSE, na = "NA")
                                             })
  output$download_analysis_1 <- downloadHandler(filename = function() "analysis_1_threshold_attainment.csv",
                                                content = function(file) {
                                                  write.csv(analysis_tables()$threshold, file, row.names = FALSE, na = "NA")
                                                })
  output$download_analysis_2 <- downloadHandler(filename = function() "analysis_2_majority_adoption.csv",
                                                content = function(file) {
                                                  write.csv(analysis_tables()$majority_adoption, file, row.names = FALSE, na = "NA")
                                                })
  output$download_series <- downloadHandler(filename = function() "simulation_timeseries.csv",
                                            content = function(file) {
                                              req(stored_series()); write.csv(stored_series(), file, row.names = FALSE, na = "NA")
                                            })
}

# APP ENTRY POINT: save as app.R and launch using Run App.
shinyApp(ui, server)
