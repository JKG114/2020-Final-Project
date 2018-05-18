library(ggplot2)
library(ggcorrplot)
library(corrplot)


#This is very similar to code where we exported csvs for matlab that contained census tract level
#demographic statistics and the races of those killed by police at the census tract level, only now
#we export 8 csvs where each csv has the same census level tract info except it corresponds to one of 
#4 regions: Northeast, South, Midwest, West.
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
total = na.omit(total)

rownames(total) <- 1:nrow(total)
stat = subset(total, select=c("State","geo_id", "TotalPop","raceethnicity", "Hispanic","White", "Black", "Native","Asian",
                              "Pacific"))


stat$raceethnicity <- as.character(stat$raceethnicity)
stat$raceethnicity[stat$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
stat$raceethnicity[stat$raceethnicity== "Native American"] <- "Native"
stat$raceethnicity[stat$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"

#stat$raceethnicity <- as.factor(stat$raceethnicity)
#create new raical category that is a combination of Asian and Pacific
stat$Asian_Pacific = stat$Asian + stat$Pacific
#stat$NonWhite = stat$Asian + stat$Pacific + stat$Native + stat$Black + stat$Hispanic

#Get the raicl demographic data for each county
Races = subset(stat, select = c("State","geo_id","TotalPop","Hispanic", "White", "Black", "Native", "Asian_Pacific"))
Races = Races[!duplicated(Races$geo_id),]
rownames(Races) <- 1:nrow(Races)

Shootings <- matrix(0, ncol = 7, nrow = nrow(Races))
Shootings <- data.frame(Shootings)
names(Shootings)[names(Shootings) == 'X1'] <- 'State'
Shootings$State = Races$State
names(Shootings)[names(Shootings) == 'X2'] <- 'geo_id'
Shootings$geo_id = Races$geo_id
names(Shootings)[names(Shootings) == 'X3'] <- 'Hispanic'
names(Shootings)[names(Shootings) == 'X4'] <- 'White'
names(Shootings)[names(Shootings) == 'X5'] <- 'Black'
names(Shootings)[names(Shootings) == 'X6'] <- 'Native'
names(Shootings)[names(Shootings) == 'X7'] <- 'Asian_Pacific'

#County the number of police killings for eachrace in each county(using geo_id)
for(i in 1:nrow(stat)) {
  if(stat$raceethnicity[i] == "Hispanic"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$Hispanic[index] <- Shootings$Hispanic[index] + 1
  }
  if(stat$raceethnicity[i] == "White"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$White[index] <- Shootings$White[index] + 1
  }
  if(stat$raceethnicity[i] == "Black"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$Black[index] <- Shootings$Black[index] + 1
  }
  if(stat$raceethnicity[i] == "Native"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$Native[index] <- Shootings$Native[index] + 1
  }
  if(stat$raceethnicity[i] == "Asian_Pacific"){
    index <- Shootings$geo_id == stat$geo_id[i]
    Shootings$Asian_Pacific[index] <- Shootings$Asian_Pacific[index] + 1
  }
  
}

Races$geo_id = NULL
#Races$NonWhite = Races$NonWhite/100
#Races$White = Races$White/100
Shootings$geo_id = NULL

#Create a dataframe sub_state with just the information we want
sub_state = subset(acs, select=c("State", "TotalPop","Hispanic", "White", "Black",
                                 "Native","Asian","Pacific" ))

#Remove rows with na's
sub_state <- na.omit(sub_state)
sub_state$Asian_Pacific = sub_state$Asian + sub_state$Pacific


#We now the Racial demographic data for each census tract and aggregate it at the state level
Races = sub_state %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            White = (sum(TotalPop*(White*.01)))/TotalState,
            Black = (sum(TotalPop*(Black*.01)))/TotalState,
            Native = (sum(TotalPop*(Native*.01)))/TotalState,
            Asian_Pacific = sum(TotalPop*(Asian_Pacific*.01))/TotalState)


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


#For Each Demographic Region We aggregate the demographic data
Demographics_NorthEast = Demographics_NorthEast %>%
  summarise(TotalPop = sum(TotalState),
            Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
            White = (sum(TotalState*(White)))/TotalPop,
            Black = (sum(TotalState*(Black)))/TotalPop,
            Native = (sum(TotalState*(Native)))/TotalPop,
            Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
Demographics_NorthEast$TotalPop = NULL

Demographics_South = Demographics_South %>%
  summarise(TotalPop = sum(TotalState),
            Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
            White = (sum(TotalState*(White)))/TotalPop,
            Black = (sum(TotalState*(Black)))/TotalPop,
            Native = (sum(TotalState*(Native)))/TotalPop,
            Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
Demographics_South$TotalPop = NULL

Demographics_Midwest = Demographics_Midwest %>%
  summarise(TotalPop = sum(TotalState),
            Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
            White = (sum(TotalState*(White)))/TotalPop,
            Black = (sum(TotalState*(Black)))/TotalPop,
            Native = (sum(TotalState*(Native)))/TotalPop,
            Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
Demographics_Midwest$TotalPop = NULL

Demographics_West = Demographics_West %>%
  summarise(TotalPop = sum(TotalState),
            Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
            White = (sum(TotalState*(White)))/TotalPop,
            Black = (sum(TotalState*(Black)))/TotalPop,
            Native = (sum(TotalState*(Native)))/TotalPop,
            Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
Demographics_West$TotalPop = NULL



#Get Rid of non-numeric values and sum shooting stats
Demographics_NorthEast$State = NULL
#Demographics_NorthEast$geo_id = NULL
Shooting_NorthEast$State = NULL
#Shooting_NorthEast$geo_id = NULL
Shooting_NorthEast = colSums(Filter(is.numeric, Shooting_NorthEast))


Demographics_South$State = NULL
#Demographics_South$geo_id = NULL
Shooting_South$State = NULL
#Shooting_South$geo_id = NULL
Shooting_South = colSums(Filter(is.numeric, Shooting_South))


Demographics_Midwest$State = NULL
#Demographics_Midwest$geo_id = NULL
Shooting_Midwest$State = NULL
#Shooting_Midwest$geo_id = NULL
Shooting_Midwest = colSums(Filter(is.numeric, Shooting_Midwest))

Demographics_West$State = NULL
#Demographics_West$geo_id = NULL
Shooting_West$State = NULL
#Shooting_West$geo_id = NULL
Shooting_West = colSums(Filter(is.numeric, Shooting_West))


#CSVs for NORTHEAST
write.csv(Shooting_NorthEast, file = "csvs/Northeeast_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_NorthEast, file = "csvs/RacesOfNortheast.csv",row.names=FALSE)

#CSVs for South
write.csv(Shooting_South, file = "csvs/South_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_South, file = "csvs/RacesOfSouth.csv",row.names=FALSE)

#CSVs for Midwest
write.csv(Shooting_Midwest, file = "csvs/Midwest_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_Midwest, file = "csvs/RacesOfMidwest.csv",row.names=FALSE)

#CSVs for West
write.csv(Shooting_West, file = "csvs/West_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_West, file = "csvs/RacesOfWest.csv",row.names=FALSE)

