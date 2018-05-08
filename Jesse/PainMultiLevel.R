library(lme4) 
library(nlme) 
library(knitr)
library(dplyr)
data <- read.csv("pain.csv")
cols <- c("age", "race2", "inccat", "treat", "sex", "nsaid", "Occupation") 
patID <- unique(data$ID)
paincol <- c(17, 19, 21, 23, 25, 27, 29)
timecol <- paincol + 1
datause <- matrix(NA,0,12)
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

datause$ID <- factor(datause$ID) 
datause$inccat <- factor(datause$inccat) 
female = datause$sex-1
white = datause$race2

#table(datause$Occupation)
# Note varied occupations. Make new variable retired (yes/no)
xx = c(grep("tired",datause$Occupation),grep("TIRED",datause$Occupation))
yy = grep("never retired",datause$Occupation)
retired = rep(0,length(datause$Occupation))
retired[xx[-which(xx %in% yy)]] = 1
retired[datause$Occupation==""] = NA
datause = data.frame(datause,retired,female,white)
datause = datause[!is.na(datause$temperature),] # Remove missing days temperature missing

## format nicer to present variables of interest dataset <- dplyr::select(datause, ID, pain, time,
dataset <- dplyr::select(datause, ID, pain, time,
                         temperature, age, inccat,
                         treat, nsaid, retired, female,
                         white)
dataset$nsaid <- as.factor(dataset$nsaid) 
dataset$treat <- as.factor(dataset$treat) 
dataset$retired <- as.factor(dataset$retired) 
dataset$female <- as.factor(dataset$female) 
dataset$white <- as.factor(dataset$white)
summary(dataset)[,-1]

