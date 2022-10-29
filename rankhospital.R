

rankhospital <- function(state, outcome, num = "best") {
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
  dat_subset <- subset(dat, State %in% state) #subset data for state
  suppressWarnings(dat_subset[, o] <- as.numeric(dat_subset[, o]))#coerce the column to numeric to use for rank and order
  dat_subset$Rank <- rank(dat_subset[, o], na.last = TRUE, ties.method = "min") #use rank function against outcome var
  dat_subset <- na.omit(dat_subset) #remove nas
  dat_subset <- dat_subset[order(dat_subset[, 47], dat_subset[, 2]),] #subset by rank then handle ties alphabetically
  dat_subset$Rank2 <- order((dat_subset[, 47]), na.last = TRUE)#re rank based
  return(dat_subset[num, 2]) #display first row, column 2
}    
?num
#test the function
best("TX", "heart attack")
best("MD", "heart attack")
best("TX", "heart failure")
best("MD", "pneumonia")
best("NY", "heart attack")
best("AL", "pneumonia")
?order
## Return hospital name in that state with lowest 30-day death rate
dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character") ## Read outcome data
dat_subset <- subset(dat, State %in% "MD") #if true the subset data for state
suppressWarnings(dat_subset[, 11] <- as.numeric(dat_subset[, 11]))#coerce the column to numeric to find minimum value
dat_subset$Rank <- rank(dat_subset[, 11], na.last = TRUE, ties.method = "min") #use rank function against outcome var
dat_subset <- na.omit(dat_subset) #remove nas
dat_subset <- dat_subset[order(dat_subset[, 47], dat_subset[, 2]),] #order by rank then alphabetically
dat_subset$Rank2 <- order((dat_subset[, 47]), na.last = TRUE)
worst <- nrow(dat_subset)
(dat_subset[worst, 2]) #display first row, column 2