rankhospital <- function(state, outcome, rank = "best") {
  ## Read outcome data
  
  data_out <- read.csv("outcome-of-care-measures.csv", colClasses= "character")
  dot<- as.data.frame(cbind(data_out[, 2],  #hospital
                            data_out[, 7],  #state
                            data_out[, 11], #heart attack
                            data_out[, 17], #heart failure
                            data_out[, 23]),#pneumonia
                      stringsAsFactors = FALSE)
  colnames(dot) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") 
  
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(!state %in% dot[ ,"state"]) {
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)){
    state_choice<- which(dot[,"state"]==state)
    state_r.choice<- dot[state_choice, ] #Extract data from state of choice
    state_r.choice[, eval(outcome)]<- as.numeric(state_r.choice[, eval(outcome)])
    state_r.choice<- state_r.choice[order(state_r.choice[, eval(outcome)], state_r.choice[, "hospital"]),]
    output<- state_r.choice[, "hospital"][rank]
    ## Return hospital name in that state with lowest 30-day death rate
  }else if(!is.numeric(rank)){
      if(rank == "best") {
          output<- best(state, outcome)
      }else if (rank == "worst"){
        state_choice<- which(dot[,"state"]==state)
        state_r.choice<- dot[state_choice, ] #Extract data from state of choice
        state_r.choice[, eval(outcome)]<- as.numeric(state_r.choice[, eval(outcome)])
        state_r.choice<- state_r.choice[order(state_r.choice[, eval(outcome)], state_r.choice[, "hospital"], decreasing = TRUE),]  
        output<- state_r.choice[, "hospital"][1]
      }else {
          stop('invalid rank')
      }
  }
  
  return(output)
}
