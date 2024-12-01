## LOADING AND FORMATTING THE OFFLINE DATA:

# Installs and load the appropriate packages

install.packages("dplyr")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(magrittr)

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

## CLEANING THE OFFLINE DATA:

# Checks for any NA's in the data set
anyNA(train)

# Changes each variable to the correct type
train_type <- train %>%
  mutate(across(c(time, posX, posY, posZ, orientation, signal, channel, type), ~ suppressWarnings(as.numeric(.))))

### DO I NEED TO CHANGE THE ORIENTATION COLUMN? WOULD IT THROW THINGS OFF?? ###
#---------------------------------------------------------------------------#

#---------------------------------------------------------------------------#

# Removes variables that provide redundant or no information
train_no_scanMac <- train_type %>% select(-scanMac,-posZ)

# Creates a function to convert the observations in the time variable from milliseconds to into POSIXct time format 
posixct_func<- function(x){  
  seconds<- x %/% 1000
  date_time<-as.POSIXct(seconds, origin = "1970-01-01", tz = "GMT")
}
# Creates a new data frame with with time converted into posixct format
train_posixct <- train_no_scanMac %>% mutate(time = posixct_func(time))

# Converts the accessPointLocations.txt file information into a table  
mac_locs <- readr::read_table("accessPointLocations.txt")

# Stores the information from the Macs column in the mac_locs table
MAC_addresses<-c(mac_locs$Macs)

# Filters the data for instances of relevant access points only
train_mac <- train_posixct %>% filter(mac %in% MAC_addresses)

# renames the Macs column to match the data set
colnames(mac_locs)[colnames(mac_locs) == "Macs"] <- "mac"

# use inner join to find the matched data effectively adding x and y columns 
train_access_points <- train_mac %>% 
  inner_join(mac_locs, by = c("mac")) %>% 
  select(
    1:5,          # Keeps the first 5 columns in place
    macX = x,     # Moves/renames the mac x coordinates to column 6 
    macY = y,     # Moves/renames the mac y coordinates to column 7
    everything()  # Moves everything else in order after that
  )

# Find the Euclidean distance between location of the device and access point
dist <- sqrt((train_access_points$posX - train_access_points$macX)^2 + (train_access_points$posY - train_access_points$macY)^2)
dist <- round(dist, digits = 2)
# Add 'dist' column after the 6th column
train_dist <- train_access_points %>%
  add_column(dist = dist, .after = 7)


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

# Checks for any NA's in the data set
anyNA(test)

# Changes each variable to the correct type
test_type <- test %>%
  mutate(across(c(time, posX, posY, posZ, orientation, signal, channel, type), ~ suppressWarnings(as.numeric(.))))

### DO I NEED TO CHANGE THE ORIENTATION COLUMN? WOULD IT THROW THINGS OFF?? ###
#---------------------------------------------------------------------------#

#---------------------------------------------------------------------------#

# Removes variables that provide redundant or no information
test_no_scanMac <- test_type %>% select(-scanMac,-posZ)

# Creates a function to convert the observations in the time variable from milliseconds to into POSIXct time format 
posixct_func<- function(x){  
  seconds<- x %/% 1000
  date_time<-as.POSIXct(seconds, origin = "1970-01-01", tz = "GMT")
}
# Creates a new data frame with with time converted into posixct format
test_posixct <- test_no_scanMac %>% mutate(time = posixct_func(time))

# Converts the accessPointLocations.txt file information into a table  
mac_locs <- readr::read_table("accessPointLocations.txt")

# Stores the information from the Macs column in the mac_locs table
MAC_addresses<-c(mac_locs$Macs)

# Filters the data for instances of relevant access points only
test_mac <- test_posixct %>% filter(mac %in% MAC_addresses)

# renames the Macs column to match the data set
colnames(mac_locs)[colnames(mac_locs) == "Macs"] <- "mac"

# use inner join to find the matched data effectively adding x and y columns 
test_access_points <- test_mac %>% 
  inner_join(mac_locs, by = c("mac")) %>% 
  select(
    1:5,          # Keeps the first 5 columns in place
    macX = x,     # Moves/renames the mac x coordinates to column 6 
    macY = y,     # Moves/renames the mac y coordinates to column 7
    everything()  # Moves everything else in order after that
  )

# Find the Euclidean distance between location of the device and access point
dist <- sqrt((test_access_points$posX - test_access_points$macX)^2 + (test_access_points$posY - test_access_points$macY)^2)
dist <- round(dist, digits = 2)
# Add 'dist' column after the 6th column
test_dist <- test_access_points %>%
  add_column(dist = dist, .after = 7)


