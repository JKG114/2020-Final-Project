library(ggplot2)
library(ggcorrplot)
library(corrplot)



#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
police = na.omit(police)
police = police[ ! police$raceethnicity %in% "Unknown", ]

acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'

#We merge the acs dataframe and police dataframe
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total = total[ ! total$raceethnicity %in% "Unknown", ]
df = na.omit(total)

rownames(df) <- 1:nrow(df)
stat = subset(df, select=c("State","geo_id", "raceethnicity", "Hispanic","White", "Black", "Native","Asian",
                           "Pacific"))


stat$raceethnicity <- as.character(stat$raceethnicity)
stat$raceethnicity[stat$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
stat$raceethnicity[stat$raceethnicity== "Native American"] <- "Native"

stat$raceethnicity[stat$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"
#stat$raceethnicity <- as.factor(stat$raceethnicity)
#stat$Asian_Pacific = stat$Asian + stat$Pacific
stat$NonWhite = stat$Asian + stat$Pacific + stat$Native + stat$Black + stat$Hispanic


Races = subset(stat, select = c("State","geo_id","NonWhite", "White"))
Races = Races[!duplicated(Races$geo_id),]
rownames(Races) <- 1:nrow(Races)

Shootings <- matrix(0, ncol = 4, nrow = 415)
Shootings <- data.frame(Shootings)
names(Shootings)[names(Shootings) == 'X1'] <- 'State'
Shootings$State=Races$State
names(Shootings)[names(Shootings) == 'X2'] <- 'geo_id'
Shootings$geo_id=Races$geo_id
names(Shootings)[names(Shootings) == 'X3'] <- 'NonWhite'
names(Shootings)[names(Shootings) == 'X4'] <- 'White'



for(i in 1:nrow(stat)) {
  if(stat$raceethnicity[i] == "Hispanic"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonWhite[index] <- Shootings$NonWhite[index] + 1
  }
  if(stat$raceethnicity[i] == "White"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$White[index] <- Shootings$White[index] + 1
  }
  if(stat$raceethnicity[i] == "Black"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonWhite[index] <- Shootings$NonWhite[index] + 1
  }
  if(stat$raceethnicity[i] == "Native"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonWhite[index] <- Shootings$NonWhite[index] + 1
  }
  if(stat$raceethnicity[i] == "Asian_Pacific"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$NonWhite[index] <- Shootings$NonWhite[index] + 1
  }
  
}
Races$geo_id = NULL
Races$NonWhite = Races$NonWhite/100
Races$White = Races$White/100
Shootings$geo_id = NULL


#I will know subset the states into regions Northeast, South, West, Midwest (I had a fifth
#but there weren't enough observations for it so I distributed the mountain states into the others)
NorthEast = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont",
              "New Jersey", "New York","Delaware","District of Columbia", "Maryland", 
              "Pennsylvania") 

South = c("Alabama", "Florida", "Georgia", "Kentucky",
          "Mississippi", "North Carolina", "South Carolina", "Tennessee","Virginia", 
          "West Virginia","Arkansas", "Louisiana","Oklahoma", "Texas")

Midwest = c("Illinois", "Indiana", "Michigan", "Minnesota", "Ohio", "Wisconsin",
            "Iowa", "Kansas", "Missouri", "Nebraska","North Dakota", "South Dakota")


West = c("Arizona", "California", "Hawaii", "Nevada","Alaska", 
         "Idaho", "Oregon", "Washington","Colorado","New Mexico","Utah","Montana","Wyoming")

Demographics_NorthEast =  subset(Races, (Races$State %in% NorthEast))
Shooting_NorthEast =  subset(Shootings, (Shootings$State %in% NorthEast))

Demographics_South = subset(Races, (Races$State %in% South))
Shooting_South =  subset(Shootings, (Shootings$State %in% South))

Demographics_Midwest = subset(Races, (Races$State %in% Midwest))
Shooting_Midwest =  subset(Shootings, (Shootings$State %in% Midwest))

Demographics_West = subset(Races, (Races$State %in% West))
Shooting_West =  subset(Shootings, (Shootings$State %in% West))

Demographics_NorthEast$State = NULL
Demographics_NorthEast$geo_id = NULL
Shooting_NorthEast$State = NULL
Shooting_NorthEast$geo_id = NULL

Demographics_South$State = NULL
Demographics_South$geo_id = NULL
Shooting_South$State = NULL
Shooting_South$geo_id = NULL


Demographics_Midwest$State = NULL
Demographics_Midwest$geo_id = NULL
Shooting_Midwest$State = NULL
Shooting_Midwest$geo_id = NULL

Demographics_West$State = NULL
Demographics_West$geo_id = NULL
Shooting_West$State = NULL
Shooting_West$geo_id = NULL

#CSVs for NORTHEAST
write.csv(Shooting_NorthEast, file = "Northeeast_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_NorthEast, file = "RacesOfNortheast.csv",row.names=FALSE)

#CSVs for South
write.csv(Shooting_South, file = "South_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_South, file = "RacesOfSouth.csv",row.names=FALSE)

#CSVs for Midwest
write.csv(Shooting_Midwest, file = "Midwest_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_Midwest, file = "RacesOfMidwest.csv",row.names=FALSE)


#CSVs for West
write.csv(Shooting_West, file = "West_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_West, file = "RacesOfWest.csv",row.names=FALSE)

