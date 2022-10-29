rm(list = ls()) # This command will clean your workspace

#define function
corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names = TRUE) #creates a list of the files
  dat <- data.frame() #create an empty data frame
  cr <- c()#create an empty vector to store results
  for (i in 1:332) {
    dat <- read.csv(files[i]) #read each file in turn
    dat <- dat[complete.cases(dat),] #subset complete observations
    if(nrow(dat)>threshold) { #test if number of rows (i.e. complete cases) is greater than threshold
      new_cr <- cor(dat$sulfate, dat$nitrate) #if yes return a correlation
    } else {
      new_cr <- vector(mode="numeric", length=0) #if no return a numeric vector of length 0
    }
    cr <- c(cr, new_cr) #append the result for each iteration of the loop
    dat <- dat[c(), ]  #empty the dataframe
  }
  return(cr)
}

#Results

cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))