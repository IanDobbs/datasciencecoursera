

best <- function(state, outcome) {
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## Read outcome data
  o <- c() ##create empty vector to store outcome column number
  if(isFALSE(state %in% unique(dat[,7]))) ## Check that state and outcome are valid
  {
    stop("invalid state")
  }
  else if(outcome %in% c("heart attack")) 
  {
    o <- 11
  }
  else if(outcome %in% c("heart failure")) 
  {
    o <- 17
  }
  else if(outcome %in% c("pneumonia")) 
  {
    o <- 23
  }
  else 
  {
    stop("invalid outcome")
  }  
  ## Return hospital name in that state with lowest 30-day death rate
  dat_subset <- subset(dat, State %in% state) #if true the subset data for state
  suppressWarnings(dat_subset[, o] <- as.numeric(dat_subset[, o]))#coerce the column to numeric to find minimum value
  dat_subset1 <- min(dat_subset[, o], na.rm = TRUE) #find minimum value for condition
  dat_subset2 <- dat_subset[dat_subset[, o] == dat_subset1,] #subset data so only those that meet condition
  dat_subset3 <- na.omit(dat_subset2) #remove nas
  dat_subset4 <- dat_subset3[order(dat_subset3[, 2]),] #if more than one answer order alphabetically
  return(dat_subset4[1, 2]) #display first row, column 2
}    

#test the function
best("TX", "heart attack")
best("MD", "heart attack")
best("TX", "heart failure")
best("MD", "pneumonia")
best("NY", "heart attack")
best("AL", "pneumonia")
