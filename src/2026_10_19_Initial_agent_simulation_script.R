#this code defines the function to run to simulate the rules with which agents update their 
#attitudes after a one on one conversation with another agent

#their attitudes are either prejudice or not prejudice


#the update rules:

#an agent has either prejudice, no prejudice or a mix of prejudice and no prejudice as their attitudes

#each interaction will contain one speaker and one listener from the population

#the speaker shares their attitude

#if the listener has the same attitude, there is no change
#if the speaker and listener have different attitudes the hearer takes on the speaker's attitude in addition to their
    #original attitude - so they have mixed attitudes
#if the hearer has mixed attitudes, then the hearer will drop the attitude that disagrees with 
#the speaker's attitude




run_model <- function(n = 100,
                      committed_share = 0.1,
                      tip_level = 0.5,
                      max_ticks = 5000) {
  
  # Names for the attitude values
  PREJUDICED     <- 0
  NOT_PREJUDICED <- 1
  BOTH           <- 2
  
  # Everyone starts with the old norm only (attitude 0)
  attitude <- rep(PREJUDICED, n)
  
  
  #Generating the committed agents:
  # Choose which agents are committed to the new norm
  n_committed   <- round(committed_share * n)
  committed_ids <- sample.int(n, n_committed)
  
  committed <- rep(FALSE, n)
  committed[committed_ids] <- TRUE
  non_committed <- !committed
  
  # Committed agents hold only the new norm (attitude 1)
  attitude[committed] <- NOT_PREJUDICED
  
  
  #the for loop to simulate the attitude update rules for each conversation:
  for (conversation_round in 1:max_ticks) {
    
    # Pick a speaker, then a different agent as the hearer
    speaker <- sample.int(n, 1)
    pool    <- setdiff(1:n, speaker)
    hearer  <- pool[sample.int(length(pool), 1)]
    
    # Speaker expresses one attitude (prejudice or non prejudice) (a two-attitude speaker picks one at random)
    prejudice <- if (attitude[speaker] == BOTH) sample(c(PREJUDICED, NOT_PREJUDICED), 1) else attitude[speaker]
    
    #if the hearer has mixed atttiudes or the attitude has the prejudice attitude
    #the hearer takes on the prejudice attitude
    if (attitude[hearer] == BOTH || attitude[hearer] == prejudice) {
      # hearer already holds the prejudice, so they keep only that one
      if (!committed[hearer]) attitude[hearer] <- prejudice
      # The speaker also collapses, unless committed
      if (!committed[speaker]) attitude[speaker] <- prejudice
    } else {
      # hearer adopts the new prejudice and now holds both, but not if the hearer is a committed agent
      if (!committed[hearer]) attitude[hearer] <- BOTH
    }
    
    # Determining the tipping point threshold
    if (mean(attitude[non_committed] == NOT_PREJUDICED) > tip_level) return(conversation_round)
  }
  
  NA   # print NA if the tipping point is never reached
}