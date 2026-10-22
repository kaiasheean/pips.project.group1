#Version_3.0_simulation and app

#Code summary:
#this code defines the function to run to simulate the rules with which agents update their 
#attitudes after a one on one conversation with another agent



#the update rules:

#their attitudes are either prejudice or not prejudice

#an agent has either prejudice, no prejudice or a mix of prejudice and no prejudice as their attitudes

#each interaction will contain one speaker and one listener from the population

#the speaker shares their attitude

#if the listener has the same attitude, there is no change
#if the speaker and listener have different attitudes the hearer takes on the speaker's attitude in addition to their
#original attitude - so they have mixed attitudes
#if the hearer has mixed attitudes, then the hearer will drop the attitude that disagrees with 
#the speaker's attitude

#NEW: every agent has a group label (committed minority or majority)
#NEW: a non committed hearer who is spoken to by a minority speaker only updates with probability 
#(1 - prejudice_discount), otherwise their attitude stays as it is


# APP: load the shiny package, which provides the functions that build the interactive web app
# (sliders, buttons, outputs, and the server logic that reacts to them)
library(shiny)
# APP: load bslib, which provides the modern page layout (page_sidebar), cards, value boxes,
# and the theme (bs_theme) that makes the app look clean and polished
library(bslib)


# ------------------------------------------------------------------
# THE AGENT BASED MODEL
# ------------------------------------------------------------------

run_model <- function(n = 100,
                      committed_share = 0.1,
                      tip_level = 0.5,
                      max_ticks = 5000,
                      prejudice_discount = 0) {   # NEW: 0 = no discount, 1 = never updates
  
  # Names for the attitude values
  anti_img <- 0
  pro_img  <- 1
  both     <- 2
  
  # NEW: Names for the group labels
  minority_label <- "minority"
  majority_label <- "majority"
  
  # Everyone starts with the old norm only (attitude 0)
  attitude <- rep(anti_img, n)
  attitude
  
  #Generating the committed agents:
  # Choose which agents are committed to the new norm
  n_committed   <- round(committed_share * n)
  committed_ids <- sample.int(n, n_committed)
  
  committed <- rep(FALSE, n)
  committed[committed_ids] <- TRUE
  non_committed <- !committed
  
  # Committed agents hold only the new norm (attitude 1)
  attitude[committed] <- pro_img
  
  # NEW: Assign group labels: committed agents get the minority label, everyone else the majority label
  group <- rep(majority_label, n)
  group[committed] <- minority_label
  
  
  #the for loop to simulate the attitude update rules for each conversation:
  for (conversation_round in 1:max_ticks) {
    
    # Pick a speaker, then a different agent as the hearer
    speaker <- sample.int(n, 1)
    pool    <- setdiff(1:n, speaker)
    hearer  <- pool[sample.int(length(pool), 1)]
    
    # Speaker expresses one attitude (prejudice or non prejudice) (a two-attitude speaker picks one at random)
    prejudice <- if (attitude[speaker] == both) sample(c(anti_img, pro_img), 1) else attitude[speaker]
    
    # NEW: If the speaker has the minority label, the hearer only updates with probability (1 - prejudice_discount)
    hearer_updates <- if (group[speaker] == minority_label) runif(1) >= prejudice_discount else TRUE
    
    #if the hearer has mixed atttiudes or the attitude has the prejudice attitude
    #the hearer takes on the prejudice attitude
    if (attitude[hearer] == both || attitude[hearer] == prejudice) {
      # hearer already holds the prejudice, so they keep only that one
      if (!committed[hearer] && hearer_updates) attitude[hearer] <- prejudice
      # The speaker also collapses, unless committed
      if (!committed[speaker]) attitude[speaker] <- prejudice
    } else {
      # hearer adopts the new prejudice and now holds both, but not if the hearer is a committed agent
      if (!committed[hearer] && hearer_updates) attitude[hearer] <- both
    }
    
    # Determining the tipping point threshold
    if (mean(attitude[non_committed] == pro_img) > tip_level) return(conversation_round)
  }
  
  NA   # print NA if the tipping point is never reached
}


# ------------------------------------------------------------------
# APP: USER INTERFACE
# ------------------------------------------------------------------

# APP: define the user interface, i.e. everything the user sees. page_sidebar() creates a page with a
# sidebar for the settings on the left and a main area for the results on the right.
ui <- page_sidebar(
  # APP: the title shown at the top of the page
  title = "Tipping points in attitude change",
  # APP: apply a clean, modern look. version = 5 uses Bootstrap 5, and bootswatch = "flatly" is a
  # ready-made colour theme (you can try "cerulean", "minty", "lux" or "darkly" for different looks).
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  # APP: build the sidebar that holds all the settings. width is in pixels.
  sidebar = sidebar(
    width = 330,
    
    # APP: a small heading above the model settings
    h5("Model settings"),
    
    # APP: slider for the number of agents. The first argument ("n") is the internal name the server uses to read
    # the value (as input$n), the second is the label the user sees. It runs from 20 to 1000 in steps of 10, and
    # starts at 100 (the default of run_model).
    sliderInput("n", "Number of agents",
                min = 20, max = 1000, value = 100, step = 10),
    
    # APP: slider for the committed group size as a percentage of all agents (0 to 50). post = "%" adds a % sign
    # after the number. The server divides the value by 100 to turn it into the proportion run_model() expects.
    sliderInput("share", "Committed group size (% of agents)",
                min = 0, max = 50, value = 15, step = 1, post = "%"),
    # APP: a line of text under the slider that shows how many agents the percentage means in practice
    # (for example "15 of 100 agents are committed"). It is filled in by output$committed_text in the server.
    textOutput("committed_text"),
    
    # APP: slider for the prejudice discount rate (a probability from 0 to 1, in steps of 0.05).
    # The default of 0.25 means a hearer ignores a committed speaker 25% of the time.
    sliderInput("discount", "Prejudice discount rate",
                min = 0, max = 1, value = 0.25, step = 0.05),
    # APP: grey explanatory text under the slider so the user understands what the number means
    helpText("0 = no discount. Higher values make non-committed agents less likely to update after hearing a committed agent. At 1 they never update, so tipping is impossible."),
    
    # THRESHOLD: slider for the tipping threshold, shown as a percentage from 10 to 90 in steps of 5.
    # The default of 50 matches the original tip_level of 0.5. The server converts it to a proportion.
    sliderInput("tip_level", "Tipping threshold (% of non-committed agents)",
                min = 10, max = 90, value = 50, step = 5, post = "%"),
    # THRESHOLD: explanatory text describing what the threshold means
    helpText("A run tips when more than this share of non-committed agents hold only the not-prejudiced attitude."),
    
    # APP: slider for the maximum number of conversation rounds a single run may last (500 to 50,000).
    # If the model has not tipped by then, the run counts as "did not tip".
    sliderInput("max_ticks", "Maximum number of rounds",
                min = 500, max = 50000, value = 5000, step = 500),
    # APP: reminder that bigger populations and higher thresholds take more rounds
    helpText("Larger populations and higher thresholds need more rounds to tip."),
    
    # APP: a thin horizontal line to visually separate the model settings from the repetition setting
    hr(),
    # APP: a second small heading
    h5("Repetitions"),
    # APP: slider for how many times the model is repeated with the same settings. One run is random,
    # so repeating it gives an estimate of how often the model tips (the tipping probability).
    sliderInput("reps", "Number of repeated runs",
                min = 10, max = 200, value = 50, step = 10),
    # APP: explanatory text for the repetitions slider
    helpText("Each run is random, so the app repeats the model to estimate how often it tips."),
    
    # APP: the button that starts the simulation. "run" is its internal name (the server reads input$run).
    # class = "btn-primary w-100" makes it the theme's main (blue) button and stretches it across the full sidebar width.
    actionButton("run", "Run simulation", class = "btn-primary w-100")
  ),
  
  # Summary boxes
  # APP: arrange the three summary boxes in a row. fill = FALSE stops the row from stretching to fill the page
  # height, so the boxes stay compact. col_widths = c(4, 4, 4) splits the 12-column page grid into three equal
  # parts (4 + 4 + 4 = 12).
  layout_columns(
    fill = FALSE,
    col_widths = c(4, 4, 4),
    # APP: first box: the proportion of repeated runs that tipped. The big number is filled in by output$tip_prob.
    value_box(title = "Tipping probability",
              value = textOutput("tip_prob"),
              theme = "primary"),
    # APP: second box: the median number of rounds taken to tip (among runs that tipped), from output$median_rounds
    value_box(title = "Median rounds to tip",
              value = textOutput("median_rounds"),
              theme = "success"),
    # APP: third box: how many agents were committed in the last run, from output$n_committed
    value_box(title = "Committed agents",
              value = textOutput("n_committed"),
              theme = "secondary")
  )
)


# ------------------------------------------------------------------
# APP: SERVER
# ------------------------------------------------------------------

# APP: define the server, i.e. the code that runs behind the scenes. It reads the user's settings from "input",
# runs the model, and sends the results to the screen through "output". "session" is required by Shiny but unused here.
server <- function(input, output, session) {
  
  # Shows how many agents the chosen percentage corresponds to
  # APP: renderText() creates text that updates automatically whenever the sliders it depends on change.
  # Here it turns the percentage (input$share) and the number of agents (input$n) into a readable sentence.
  # round() gives a whole number of agents, matching what run_model() does with round(committed_share * n).
  output$committed_text <- renderText({
    paste0(round(input$share / 100 * input$n), " of ", input$n, " agents are committed")
  })
  
  # Runs the model only when the button is clicked (and once when the app opens)
  # APP: eventReactive() stores a result that is recalculated only when the event in its first argument happens,
  # here a click on the "Run simulation" button (input$run). This means moving a slider does NOT start a run, so the
  # user can set up several settings and then run once. The result is stored in "results", which the outputs use below.
  results <- eventReactive(input$run, {
    # APP: copy the slider values into short variable names. These are read once when the button is clicked,
    # so the results always match the settings used for that run.
    n     <- input$n
    # APP: the committed slider is in percent (0 to 50), but run_model() needs a proportion (0 to 0.5), so divide by 100
    share <- input$share / 100
    d     <- input$discount
    # THRESHOLD: the threshold slider is in percent, but run_model() needs a proportion, so divide by 100
    tip   <- input$tip_level / 100
    mt    <- input$max_ticks
    reps  <- input$reps
    
    # APP: withProgress() shows a progress bar while the code inside runs, so the user knows the simulation is working.
    # value = 0 means the bar starts empty. Whatever the block returns at the end is stored in "ticks".
    ticks <- withProgress(message = "Running simulation...", value = 0, {
      # repeated runs, keeping only the tipping round (NA = never tipped)
      # APP: repeat the model "reps" times. vapply() loops over 1, 2, ..., reps and collects one number from each run
      # into a vector (the numeric(1) tells it to expect exactly one number each time).
      vapply(seq_len(reps), function(i) {
        # APP: move the progress bar forward by 1/reps, so it reaches full after the last repeat
        incProgress(1 / reps)
        # APP: as.numeric() converts a never-tipped result (a logical NA) into a numeric NA, because vapply()
        # requires every result to be a number.
        as.numeric(run_model(n = n, committed_share = share, tip_level = tip,
                             max_ticks = mt, prejudice_discount = d))
      }, numeric(1))
    })
    
    # APP: return everything the summary boxes need in one list: the tipping rounds of all repeated runs, plus the
    # number of agents and committed share used (stored so the boxes describe the run that was actually done,
    # not the sliders' current positions).
    list(ticks = ticks, n = n, share = share)
    # APP: ignoreNULL = FALSE makes the model also run once when the app first opens (before the button has been
    # clicked), so the user sees results straight away instead of empty boxes.
  }, ignoreNULL = FALSE)
  
  # Summary boxes
  # APP: the tipping probability box. !is.na(r$ticks) is TRUE for runs that tipped, and mean() of those TRUE/FALSE
  # values is the proportion that tipped. It is multiplied by 100 and shown as a whole-number percentage with sprintf().
  output$tip_prob <- renderText({
    r <- results()
    sprintf("%.0f%%", 100 * mean(!is.na(r$ticks)))
  })
  
  # APP: the median rounds box. First keep only the runs that tipped (drop the NAs), because a run that never
  # tipped has no tipping time to include.
  output$median_rounds <- renderText({
    tipped <- results()$ticks
    tipped <- tipped[!is.na(tipped)]
    # APP: if no run tipped there is no median, so show a message; otherwise show the median rounded to a whole number,
    # with a thousands separator (for example 1,250).
    if (length(tipped) == 0) "No runs tipped" else format(round(median(tipped)), big.mark = ",")
  })
  
  # APP: the committed agents box: shows the number of committed agents out of the total, using the settings from the last run
  output$n_committed <- renderText({
    r <- results()
    paste0(round(r$share * r$n), " of ", r$n)
  })
}

# APP: combine the user interface and the server into a running Shiny app. This is the line that starts the app
# when you click "Run App" in RStudio or call shiny::runApp().
shinyApp(ui, server)