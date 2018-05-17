library(nlme) 
library(lme4)
library(knitr) 
library(dplyr)
setwd("/Users/stuartgeman/Desktop/data2020/Final Project")
data <- read.csv("pain.csv")
cols <- c("age", "race2", "inccat", "treat", "sex", "nsaid", "Occupation")
patID <- unique(data$ID)
paincol <- c(17, 19, 21, 23, 25, 27, 29)
timecol <-paincol+1
datause <-matrix(NA,0,12)
# Set up database with pain and temperature measurements extracted on dates when pain meas
for (i in 1:length(patID)) {
  id <- patID[i]
  tempdt <- data[data$ID==id, ]
  zi <- tempdt[1, c("ID", cols)]
  pain <- tempdt[1, paincol]
  pain <- pain[!is.na(pain)]
  time <- tempdt[1, timecol]
  temperature <- tempdt$avgtemp[tempdt$WeatherDate %in% time] 
  time <- time[!is.na(time)]
  time <- time - time[1]
  datapti <- cbind(pain, time, temperature) 
  datapti <- cbind(zi, datapti)
  datause <- rbind(datause, datapti)
}