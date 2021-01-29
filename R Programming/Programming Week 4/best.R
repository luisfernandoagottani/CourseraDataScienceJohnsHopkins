##Finding the best hospital in a state

best <- function(state, outcome) {
  ## Read outcome data
  
  data_out <- read.csv("outcome-of-care-measures.csv", colClasses= "character", header = T)
  dot<- as.data.frame(cbind(data_out[, 2],  #hospital
                            data_out[, 7],  #state
                            data_out[, 11], #heart attack
                            data_out[, 17], #heart failure
                            data_out[, 23]),#pneumonia
                            stringsAsFactors = FALSE)
  colnames(dot) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")  
## Check that state and outcome are valid
if(!state %in% dot[ ,"state"]) {
      stop('invalid state')
} else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
      stop('invalid outcome')
} else {
  state_choice<- which(dot[,"state"]==state)
  state_r.choice<- dot[state_choice, ] #Extract data from state of choice
  result_dot<- as.numeric(state_r.choice[, (outcome)])
  result_dot_min<- min(result_dot, na.rm=TRUE)
  result_final<- state_r.choice[, "hospital"][which(result_dot==result_dot_min)]
  output<- result_final[order(result_final)] #Order by Name
  ## Return hospital name in that state with lowest 30-day death rate
       }      
return(output)
}