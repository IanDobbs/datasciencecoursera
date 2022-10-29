rm(list = ls()) # This command will clean your workspace

# define function
complete <- function(directory, id = 1:332) {
files <- list.files(directory, full.names = TRUE) #creates a list of the files
dat <- data.frame() #create an empty data frame
cc <- c() #create empty vectors to store results
monitor <- c()
for (i in 1:332) {
  dat <- read.csv(files[i]) #read each file in turn
    if((nrow(na.omit(dat))) > 0) { #test if number of rows (i.e. complete cases) is greater than zero
    new_cc <- nrow(na.omit(dat)) # if yes return nrow for complete cases
    new_monitor <- i 
  } else {
    new_cc <- 0 #else 0
    new_monitor <- i 
  }
  cc <- c(cc, new_cc)
  monitor <- c(monitor, new_monitor) #append the results for each iteration of the loop
    dat <- dat[c(), ]  #empty the dataframe
  }
dat_subset <- cbind(id=monitor, nobs=cc)
nobs_subset <- (dat_subset[id,]) #[rows=id, columns=all]
nobs_subset
}

#Results
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc[, 'nobs'])
cc <- complete("specdata", 54)
cc <- as.data.frame(t(cc))
print(cc$nobs) # not sure why this errors? vector needs converting to data frame and transposing and it works

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])