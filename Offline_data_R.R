#_______________________________________________________________________________#
#_______________________________________________________________________________#
## LOADING, FORMATTING, AND CLEANING THE OFFLINE DATA:
#_______________________________________________________________________________#
#_______________________________________________________________________________#

# Installs and load the appropriate packages
install.packages("dplyr")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
#-------------------------------------------------------------------------------#
# sets the current working directory
setwd("C:/Users/risin/OneDrive/Desktop/School/Classes/STAT410/IPS_data")
#-------------------------------------------------------------------------------#
# Loads the offline.final.trace.txt into an object
offline_data <- readLines("offline.final.trace.txt")
#-------------------------------------------------------------------------------#
# offline_data is a character vector of length 151392
str(offline_data)
#-------------------------------------------------------------------------------#
# There are 5312 comments in the data
sum(substr(offline_data,1,1) =="#")
#-------------------------------------------------------------------------------#
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
#-------------------------------------------------------------------------------#
# Runs the porcessLine function over the entire data set to build a data frame
lines <- offline_data[substr(offline_data,1,1) != "#"] # Removes comments from data
tmp <- lapply(lines,processLine) 
train <- as.data.frame(do.call("rbind",tmp), stringAsFactors=F)
#-------------------------------------------------------------------------------#
# Check dimensions of training set
dim(train)
head(train)
str(train)
#-------------------------------------------------------------------------------#
# Assigns names to each of the columns
names(train) <- c("time","scanMac","posX","posY","posZ","orientation","mac","signal","channel","type")
#-------------------------------------------------------------------------------#
# Checks for any NA's in the data set
anyNA(train)
#-------------------------------------------------------------------------------#
# Changes each variable to the correct type
train_type <- train %>%
  mutate(across(c(time, posX, posY, posZ, orientation, signal, channel, type), ~ suppressWarnings(as.numeric(.))))
#-------------------------------------------------------------------------------#
# Removes variables that provide redundant or no information
train_no_scanMac <- train_type %>% select(-scanMac,-posZ)
#-------------------------------------------------------------------------------#
# Creates a function to convert the observations in the time variable from milliseconds to into POSIXct time format 
posixct_func<- function(x){  
  seconds<- x %/% 1000
  date_time<-as.POSIXct(seconds, origin = "1970-01-01", tz = "GMT")
}
#-------------------------------------------------------------------------------#
# Creates a new data frame with with time converted into posixct format
train_posixct <- train_no_scanMac %>% mutate(time = posixct_func(time))
#-------------------------------------------------------------------------------#
# Converts the accessPointLocations.txt file information into a table  
mac_locs <- readr::read_table("accessPointLocations.txt")
#-------------------------------------------------------------------------------#
# Stores the information from the Macs column in the mac_locs table
MAC_addresses<-c(mac_locs$Macs)
#-------------------------------------------------------------------------------#
# Filters the data for instances of relevant access points only
train_mac <- train_posixct %>% filter(mac %in% MAC_addresses)
#-------------------------------------------------------------------------------#
# renames the Macs column to match the data set
colnames(mac_locs)[colnames(mac_locs) == "Macs"] <- "mac"
#-------------------------------------------------------------------------------#
# use inner join to find the matched data effectively adding x and y columns 
train_access_points <- train_mac %>% 
  inner_join(mac_locs, by = c("mac")) %>% 
  select(
    1:5,          # Keeps the first 5 columns in place
    macX = x,     # Moves/renames the mac x coordinates to column 6 
    macY = y,     # Moves/renames the mac y coordinates to column 7
    everything()  # Moves everything else in order after that
  )
#-------------------------------------------------------------------------------#
### DO I NEED TO CHANGE THE ORIENTATION COLUMN? WOULD IT THROW THINGS OFF?? ###
#-------------------------------------------------------------------------------#
# This function takes and an angle and locates its closest proximity to angles in the bin vector
nearest_angle <- function(angle, bins) {
  bins[which.min(abs(angle %% 360 - bins))]
}
bins <- c(0, 45, 90, 135, 180, 225, 270, 315)
#-------------------------------------------------------------------------------#
# Applies the nearest_angle function to the orientation column and creates a group version of the orientation variable conformed to angles 0, 45, 90, 135, 180, 225, 270, 315.
train_orientation <- train_access_points %>%
  transmute(
    across(1:3),                  # Retain the first three columns
    adj_orient = sapply(orientation, nearest_angle, bins = bins),  # Create the new column
    across(-orientation)          # Retain all other columns except 'orientation'
  )
#-------------------------------------------------------------------------------#
# Consolidates useful information by groups and performs summary statistics   
train_summary <- train_orientation %>%
  group_by(posX, posY, adj_orient, mac , macX, macY) %>%
  summarise(mean_signal = mean(signal, na.rm = TRUE),
            median_signal = median(signal, na.rm = TRUE),
            sd_signal = sd(signal, na.rm = TRUE),
            min_signal = min(signal, na.rm = TRUE),
            max_signal = max(signal, na.rm = TRUE),
            IQR_signal = IQR(signal, na.rm = TRUE),
            Q1 = quantile(signal, 0.25, na.rm = TRUE),        
            Q3 = quantile(signal, 0.75, na.rm = TRUE),
            IQR_min = Q1 - 1.5 * IQR_signal,          
            IQR_max = Q3 + 1.5 * IQR_signal,
            outliers = sum(signal < IQR_min |      # Counts outliers
                             signal > IQR_max),
            count = n(),
            .groups = "drop"  # Removes grouping from the resulting data frame
  )%>% # Removes information no longer needed after getting the ouliers
  select(-min_signal,-max_signal,-IQR_signal,-Q1,-Q3,-IQR_min,-IQR_max)
#-------------------------------------------------------------------------------#
# Find the Euclidean distance between location of the device and access point
dist <- sqrt((train_summary$posX - train_summary$macX)^2 + (train_summary$posY - train_summary$macY)^2)
dist <- round(dist, digits = 2)
#-------------------------------------------------------------------------------#
# Add 'dist' column after the 6th column
train_dist <- train_summary %>%
  add_column(dist = dist, .after = 7)
#-------------------------------------------------------------------------------#
plot(train_dist$median_signal, train_dist$dist, main = "Offline Distance vs Signal", xlab = "Offline_Signal", ylab = "Offline_Distance", pch = 16)
#-------------------------------------------------------------------------------#
train_closestAPs <- train_dist %>%
  group_by(posX, posY, mac) %>%                 # Group by position and router
  filter(mean_signal == min(mean_signal)) %>%   # Keep the recording with the strongest signal per router
  ungroup() %>%                                 # Remove grouping to avoid interference
  group_by(posX, posY) %>%                      # Regroup by position
  arrange(dist, .by_group = TRUE) %>%           # Sort by distance
  distinct(mac, .keep_all = TRUE) %>%           # Keep only one row per unique router
  slice(1:3)      
#-------------------------------------------------------------------------------#
# Rewrites the new information over the original testing data for ease of code
train <- train_closestAPs
print(train,n = 50, width = Inf)
#-------------------------------------------------------------------------------#
# Plot the relationship between signal strength and distance
plot(train$median_signal, train$dist, main = "Offline Distance vs Signal", xlab = "Offline_Signal", ylab = "Offline_Distance", pch = 16)
#_______________________________________________________________________________#
#_______________________________________________________________________________#
## LOADING, FORMATTING, AND CLEANING THE ONLINE DATA:
#_______________________________________________________________________________#
#_______________________________________________________________________________#

# Loads the online.final.trace.txt into an object
online_data <- readLines("online.final.trace.txt")
#-------------------------------------------------------------------------------#
# online_data is a character vector of length 6832
str(online_data)
#-------------------------------------------------------------------------------#
# There are 240 comments in the data
sum(substr(online_data,1,1) =="#")
#-------------------------------------------------------------------------------#
# Runs the porcessLine function over the entire data set to build a data frame
lines <- online_data[substr(online_data,1,1) != "#"] # Removes comments from data
tmp <- lapply(lines,processLine) 
test <- as.data.frame(do.call("rbind",tmp), stringAsFactors=F)
#-------------------------------------------------------------------------------#
# Check dimensions of training set
dim(test)
head(test)
str(test)
#-------------------------------------------------------------------------------#
# Assigns names to each of the columns
names(test) <- c("time","scanMac","posX","posY","posZ","orientation","mac","signal","channel","type")
#-------------------------------------------------------------------------------#
# Checks for any NA's in the data set
anyNA(test)
#-------------------------------------------------------------------------------#
# Changes each variable to the correct type
test_type <- test %>%
  mutate(across(c(time, posX, posY, posZ, orientation, signal, channel, type), ~ suppressWarnings(as.numeric(.))))
#-------------------------------------------------------------------------------#
# Removes variables that provide redundant or no information
test_no_scanMac <- test_type %>% select(-scanMac,-posZ)
#-------------------------------------------------------------------------------#
# Creates a function to convert the observations in the time variable from milliseconds to into POSIXct time format 
posixct_func<- function(x){  
  seconds<- x %/% 1000
  date_time<-as.POSIXct(seconds, origin = "1970-01-01", tz = "GMT")
}
# Creates a new data frame with with time converted into posixct format
test_posixct <- test_no_scanMac %>% mutate(time = posixct_func(time))
#-------------------------------------------------------------------------------#
# Converts the accessPointLocations.txt file information into a table  
mac_locs <- readr::read_table("accessPointLocations.txt")
#-------------------------------------------------------------------------------#
# Stores the information from the Macs column in the mac_locs table
MAC_addresses<-c(mac_locs$Macs)
#-------------------------------------------------------------------------------#
# Filters the data for instances of relevant access points only
test_mac <- test_posixct %>% filter(mac %in% MAC_addresses)
#-------------------------------------------------------------------------------#
# renames the Macs column to match the data set
colnames(mac_locs)[colnames(mac_locs) == "Macs"] <- "mac"
#-------------------------------------------------------------------------------#
# use inner join to find the matched data effectively adding x and y columns 
test_access_points <- test_mac %>% 
  inner_join(mac_locs, by = c("mac")) %>% 
  select(
    1:5,          # Keeps the first 5 columns in place
    macX = x,     # Moves/renames the mac x coordinates to column 6 
    macY = y,     # Moves/renames the mac y coordinates to column 7
    everything()  # Moves everything else in order after that
  )
#-------------------------------------------------------------------------------#
# This function takes and an angle and locates its closest proximity to angles in the bin vector
nearest_angle <- function(angle, bins) {
  bins[which.min(abs(angle %% 360 - bins))]
}
bins <- c(0, 45, 90, 135, 180, 225, 270, 315)
#-------------------------------------------------------------------------------#
### DO I NEED TO CHANGE THE ORIENTATION COLUMN? WOULD IT THROW THINGS OFF?? ###
#-------------------------------------------------------------------------------#
# Applies the nearest_angle function to the orientation column and creates a group version of the orientation variable conformed to angles 0, 45, 90, 135, 180, 225, 270, 315.
test_orientation <- test_access_points %>%
  transmute(
    across(1:3),                  # Retain the first three columns
    adj_orient = sapply(orientation, nearest_angle, bins = bins),  # Create the new column
    across(-orientation)          # Retain all other columns except 'orientation'
  )
#-------------------------------------------------------------------------------#
test_summary <- test_orientation %>%
  group_by(posX, posY, adj_orient, mac , macX, macY) %>%
  summarise(mean_signal = mean(signal, na.rm = TRUE),
            median_signal = median(signal, na.rm = TRUE),
            sd_signal = sd(signal, na.rm = TRUE),
            min_signal = min(signal, na.rm = TRUE),
            max_signal = max(signal, na.rm = TRUE),
            IQR_signal = IQR(signal, na.rm = TRUE),
            Q1 = quantile(signal, 0.25, na.rm = TRUE),        
            Q3 = quantile(signal, 0.75, na.rm = TRUE),
            IQR_min = Q1 - 1.5 * IQR_signal,          
            IQR_max = Q3 + 1.5 * IQR_signal,
            outliers = sum(signal < IQR_min |      # Count outliers
                             signal > IQR_max),
            count = n(),
            .groups = "drop"  # Removes grouping from the resulting data frame
  )%>% 
  select(-min_signal,-max_signal,-IQR_signal,-Q1,-Q3,-IQR_min,-IQR_max)
#-------------------------------------------------------------------------------#
# Find the Euclidean distance between location of the device and access point
dist <- sqrt((test_summary$posX - test_summary$macX)^2 + (test_summary$posY - test_summary$macY)^2)
dist <- round(dist, digits = 2)
#-------------------------------------------------------------------------------#
# Add 'dist' column after the 6th column
test_dist <- test_summary %>%
  add_column(dist = dist, .after = 7)
#-------------------------------------------------------------------------------#
plot(train_dist$median_signal, train_dist$dist, main = "Online Distance vs Signal", xlab = "Online_Signal", ylab = "Online_Distance", pch = 16)
#-------------------------------------------------------------------------------#
test_closestAPs <- test_dist %>%
  group_by(posX, posY, mac) %>%                 # Group by position and router
  filter(mean_signal == max(mean_signal)) %>%   # Keep the recording with the strongest signal per router
  ungroup() %>%                                 # Remove grouping to avoid interference
  group_by(posX, posY) %>%                      # Regroup by position
  arrange(dist, .by_group = TRUE) %>%           # Sort by distance
  distinct(mac, .keep_all = TRUE) %>%           # Keep only one row per unique router
  slice(1:3)      
#-------------------------------------------------------------------------------#
# Rewrites the new information over the original testing data for ease of code
test <- test_closestAPs
print(test,n = 50, width = Inf)
#-------------------------------------------------------------------------------#
# Plots the new data frame with some noise removed from the data
plot(train$median_signal, train$dist, main = "Online Distance vs Signal", xlab = "Online_Signal", ylab = "Online_Distance", pch = 16)
#_______________________________________________________________________________#
#_______________________________________________________________________________#
## PREDICTION MODELING
#_______________________________________________________________________________#
#_______________________________________________________________________________#

### DO WE NEED TO NORMALIZE THE PREDICTOR VARIABLES??? (DIST IN THIS CASE) 
#-------------------------------------------------------------------------------#
#normalize <- function(x) (x - min(x)) / (max(x) - min(x))
#train_macX_normalized <- as.data.frame(lapply(macX, normalize))
#-------------------------------------------------------------------------------#

# Load libraries
install.packages("FNN")
install.packages("caret")
library(FNN)
library(caret)

# Ensure train$signal and test$signal are matrices/data frames, not vectors
train_signal <- as.data.frame(train$median_signal)
test_signal <- as.data.frame(test$median_signal)

# Train KNN model
k <- 9
knn_model <- knn.reg(train = train_signal, test = test_signal, y = train$dist, k = k)

knn_y_predictions <- knn_model$pred
knn_y_values <- test$dist
knn_residuals <- knn_y_values - knn_y_predictions
knn_sd_resid <- sd(knn_residuals)

print(knn_residuals)
print(knn_sd_resid)

plot(knn_y_predictions,abs(knn_residuals))
#-------------------------------------------------------------------------------#

#train_S <- train %>% mutate(median_signal = -1 * median_signal)
#S <- train_S$median_signal

#nls_model <- nls(dist ~ a1 + a2 * log(S), start = list(a1 = 1, a2 = -1), data = train_S)

#nls_residuals <- residuals(model)

#nls_sd_resid <- sd(model1_residuals)


#-------------------------------------------------------------------------------#


