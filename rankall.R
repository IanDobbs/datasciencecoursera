library(dplyr)

rankall <- function(outcome, num = "best") 
{
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  o <- c() ##create empty vector to store outcome column number
  
  ## Check the outcome is valid
  if(outcome %in% c("heart attack")) 
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
  
  ## for each state, find the hospital of the given rank
  dat[, o] <- as.numeric(dat[, o])#coerce the column to numeric to use for order
  
  dat <- na.omit(dat[order(dat[7], dat[o], dat[2]),]) ## remove nas and order
  
  states_list <- split(dat, f = dat$State) ## split into states
  
  new_names <- as.character(unique(dat[,7]))
  
  datalist = vector("list", length = length(new_names))
  
  for (i in 1:length(states_list))
  {
    states_list[[i]][47] <- 1:nrow(states_list[[i]]) ## index/rank
    
    if(num %in% c("worst"))
    {
      num1 <- nrow(states_list[[i]])
    }
    else if(num %in% c("best"))
    {
      num1 <- 1
    }
    else
    {
      num1 <- num
    }
    dat1 <- states_list[[i]][states_list[[i]][47]==num1, ]
    datalist[[i]] <- dat1
  }
  
  result = do.call(rbind, datalist)
  
  result = subset(result, select = c(Hospital.Name, State))
  
  states <- data.frame(new_names)
  
  states <- states %>% rename(State=1)
  
  ##return a data frame with the hospital names and the abbreviated state name
  
  final_result <- merge(states, result, by = "State", all = TRUE)
  
  final_result
  
}    

## test results

head(rankall("heart attack",20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure", 1), 10)

## debug(rankall)