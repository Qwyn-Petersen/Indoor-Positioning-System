# sets the current working directory
setwd("C:/Users/risin/OneDrive/Desktop/School/Classes/STAT410/IPS_data")

# Loads the offline.final.trace.txt into an object
offline_data <- readLines("offline.final.trace.txt")

# offline_data is a character vector of length 151392
str(offline_data)

# There are 5312 comments in the data
sum(substr(offline_data,1,1) =="#")

# This function splits the text line to match specific columns names its respective row of data
processLine <- function(x){
  # Regex in the strsplit function to split data on ;=, characeters (from looking at the data)
  tokens = strsplit(x,"[;=,]")[[1]]
  # Handles the case where there are no recorded signals (not useful for this project) and removes the data                           
  if (length(tokens) == 10){
    return(NULL)
  }
  # For each signal recording, tokens 1,3,5, and 9 are columns names
  tmp = matrix(tokens[-(1:10)],ncol=4, byrow=T)
  cbind(matrix(tokens[c(2,4,6:8,10)],nrow=nrow(tmp),ncol=6,byrow=T),tmp)
}

# Runs the porcessLine function over the entire data set to build a data frame
lines <- offline_data[substr(offline_data,1,1) != "#"] # Removes comments from data
tmp <- lapply(lines,processLine) 
train <- as.data.frame(do.call("rbind",tmp), stringAsFactors=F)

# Check dimensions of training set
dim(train)
head(train)
str(train)

# Assigns names to each of the columns
names(train) <- c("time","scanMac","posX","posY","posZ","orientation","mac","signal","channel","type")

### MODIFY THE DATA FRAME AS NECESSARY
####
#####

# Loads the online.final.trace.txt into an object
online_data <- readLines("online.final.trace.txt")

# online_data is a character vector of length 6832
str(online_data)

# There are 240 comments in the data
sum(substr(online_data,1,1) =="#")

# Runs the porcessLine function over the entire data set to build a data frame
lines <- online_data[substr(online_data,1,1) != "#"] # Removes comments from data
tmp <- lapply(lines,processLine) 
test <- as.data.frame(do.call("rbind",tmp), stringAsFactors=F)

# Check dimensions of training set
dim(test)
head(test)
str(test)

# Assigns names to each of the columns
names(test) <- c("time","scanMac","posX","posY","posZ","orientation","mac","signal","channel","type")





