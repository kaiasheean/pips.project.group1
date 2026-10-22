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

#NEW: every agent has a group label (committed minority or majority)
#NEW: a non committed hearer who is spoken to by a minority speaker only updates with probability 
#(1 - prejudice_discount), otherwise their attitude stays as it is




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