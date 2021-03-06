library(ggplot2)
library(ggcorrplot)
library(corrplot)



#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total = total[ ! total$raceethnicity %in% "Unknown", ]

total = na.omit(total)
rownames(total) <- 1:nrow(total)
stat = subset(total, select=c("geo_id", "raceethnicity", "Hispanic","White", "Black", "Native","Asian",
                              "Pacific"))

#Recategorize data as strings/Rename To Match ACS
stat$raceethnicity <- as.character(stat$raceethnicity)
stat$raceethnicity[stat$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
stat$raceethnicity[stat$raceethnicity== "Native American"] <- "Native"
stat$raceethnicity[stat$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"

#stat$raceethnicity <- as.factor(stat$raceethnicity)
stat$NonHispanic = stat$White + stat$Black +stat$Asian + stat$Pacific

Races = subset(stat, select = c("geo_id","NonHispanic","Hispanic"))
Races = Races[!duplicated(Races$geo_id),]
rownames(Races) <- 1:nrow(Races)

Shootings <- matrix(0, ncol = 3, nrow = 415)
Shootings <- data.frame(Shootings)
names(Shootings)[names(Shootings) == 'X1'] <- 'geo_id'
Shootings$geo_id=Races$geo_id
names(Shootings)[names(Shootings) == 'X2'] <- 'NonHispanic'
names(Shootings)[names(Shootings) == 'X3'] <- 'Hispanic'



for(i in 1:nrow(stat)) {
  if(stat$raceethnicity[i] == "Hispanic"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$Hispanic[index] <- Shootings$Hispanic[index] + 1
  }
  if(stat$raceethnicity[i] == "White"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonHispanic[index] <- Shootings$NonHispanic[index] + 1
  }
  if(stat$raceethnicity[i] == "Black"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonHispanic[index] <- Shootings$NonHispanic[index] + 1
  }
  if(stat$raceethnicity[i] == "Native"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonHispanic[index] <- Shootings$NonHispanic[index] + 1
  }
  if(stat$raceethnicity[i] == "Asian_Pacific"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonHispanic[index] <- Shootings$NonHispanic[index] + 1
  }
  
}
Races$geo_id = NULL
#Make Fractions
Races = Races/100

Shootings$geo_id = NULL

# Write CSV's of Shootings and Races dataframes to export to Matlab
write.csv(Shootings, file = "csvs/Hispanic_Binary_Tract_RacesOfVictims.csv",row.names=FALSE)
write.csv(Races, file = "csvs/Hispanic_Binary_Tract_RacesOfCounties.csv",row.names=FALSE)


