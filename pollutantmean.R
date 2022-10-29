rm(list = ls()) # This command will clean your workspace

#define function
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(dataset_url, "specdata.zip")
unzip("specdata.zip")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory, full.names = TRUE) #creates a list of the files
  dat <- data.frame() #creates an empty data frame
  for (i in 1:332) {
    dat <- rbind(dat, read.csv(files[i]))
  }
  dat_subset <- dat[which(dat[, "ID"] %in% id),] #use the operator %in% to subset the DataFrame rows based on a list of values
  mean(dat_subset[, pollutant], na.rm = TRUE)
  
}

#Results

pollutantmean("specdata", "sulfate", id=1:10)
pollutantmean("specdata", "nitrate", id=70:72)
pollutantmean("specdata", "nitrate", id=23)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
