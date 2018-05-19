library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 

#The First part of the code below is the same as the code in the Eda_For_Matlab_State_Test.R
#file. It diverges when we start subsetting the data by our defined regions. We then aggregate the 
#data for each region. In This code we take the aggregated regional Data, i.e. we get the aggregated
#statistics over an entire region. 

#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
police = na.omit(police)
police = police[ ! police$raceethnicity %in% "Unknown", ]

acs = read.csv("acs2015_census_tract_data.csv")
#Rename column to make a merge possible
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
#We're going to select geo_id and state, so we merge state from acs with police
#so the state column in police so it has full state name
states = subset(acs, select=c("geo_id", "State"))
police <- merge(police, states, by = "geo_id")
#get rid of the original state colummn to avoid confusion
police$state <-NULL

#PART 1: CREATE A DATAFRAME WHERE EACH ROW CORRESPONDS TO A STATE AND THE COLUMNS HAVE RACIAL 
#CATEGORIES. ENTRIES: NUM OF THAT RACE SHOT IN THE STATE DIVIDED BY TOTAL SHOT IN STATE

#We will create a new dataframe with the race of the victim the state it occured in (perhaps later
#we'll consider other variable e.g., armed). 
state_killed <- subset(police, select=c("geo_id", "State","raceethnicity" ))
#A new row with the number of killings in each state
state_killed <- transform(state_killed, freq.loc = ave(seq(nrow(state_killed)), State, FUN=length))
#Change Raceethnicity to character from factor and State
state_killed$raceethnicity <- as.character(state_killed$raceethnicity)
state_killed$State <- as.character(state_killed$State)
#change race categories to match acs
state_killed$raceethnicity[state_killed$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
state_killed$raceethnicity[state_killed$raceethnicity== "Native American"] <- "Native"
state_killed$raceethnicity[state_killed$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"


#We create a matrix with a row for each state with a shooting and a column for each race and geo_id
#We'll record the number of shootings for each race in that state
Shootings <- matrix(0, ncol = 6, nrow = 47)
Shootings <- data.frame(Shootings)
names(Shootings)[names(Shootings) == 'X1'] <- 'State'
Shootings$State = unique(state_killed$State)
names(Shootings)[names(Shootings) == 'X2'] <- 'Hispanic'
names(Shootings)[names(Shootings) == 'X3'] <- 'White'
names(Shootings)[names(Shootings) == 'X4'] <- 'Black'
names(Shootings)[names(Shootings) == 'X5'] <- 'Native'
names(Shootings)[names(Shootings) == 'X6'] <- 'Asian_Pacific'

#Iterate through the state_killed dataframe and record the race of the deceased for each killing
#For each state we will have the fraction of each race killed by police 
for(i in 1:nrow(state_killed)) {
  if(state_killed$raceethnicity[i] == "Hispanic"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Hispanic[index] <- Shootings$Hispanic[index] + 1 #(1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "White"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$White[index] <- Shootings$White[index] + 1 #(1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Black"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Black[index] <- Shootings$Black[index] + 1 #(1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Native"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Native[index] <- Shootings$Native[index] + 1 #(1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Asian_Pacific"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Asian_Pacific[index] <- Shootings$Asian_Pacific[index] + 1 #(1/state_killed$freq.loc[i])
  }
}



#PART 2: CREATE A DATAFRAME WHERE EACH ROW CORRESPONDS TO A STATE AND THE COLUMNS HAVE RACIAL 
#CATEGORY. ENTRIES: FRACTION OF STATE THAT IS OF THAT RACE

#Create a dataframe sub_state with just the information we want
sub_state = subset(acs, select=c("State", "TotalPop","Hispanic", "White", "Black",
                                 "Native","Asian","Pacific" ))

#Remove rows with na's
sub_state <- na.omit(sub_state)

#calculate the racial demographics (notice that for Asian and Pacific we haven't done this since
#we will need to combine them into a race later)
Demographics = sub_state %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            White = (sum(TotalPop*(White*.01)))/TotalState,
            Black = (sum(TotalPop*(Black*.01)))/TotalState,
            Native = (sum(TotalPop*(Native*.01)))/TotalState,
            TotalAsian = sum(TotalPop*(Asian*.01)),
            TotalPacific = sum(TotalPop*(Pacific*.01)))

#Calculate the fraction of each state that's Asian/Pacific
Demographics$Asian_Pacific = (Demographics$TotalAsian + 
                                Demographics$TotalPacific)/Demographics$TotalState
#Get Rid of the Obselete columns
Demographics$TotalAsian = NULL
Demographics$TotalPacific = NULL
#This time we keep the total state so that we can than aggregate the data across each region.
#Demographics$TotalState = NULL

# If we wanted to add a new column, NumKilled, with number killed in Each State we could uncomment
#NumKilled = unique(subset(state_killed, select=c("freq.loc", "State")))
#Demographics = merge(Demographics, NumKilled, by = "State")

# We'll remove the rows from the demographics dataframe for states not in the shootings dataframe
Demographics =  subset(Demographics, (Demographics$State %in% Shootings$State))

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

#Mountain = c("New Mexico", "Colorado", "Uta", "Montana", "Wyoming","South Dakota", "North Dakota")

West = c("Arizona", "California", "Hawaii", "Nevada","Alaska", 
         "Idaho", "Oregon", "Washington","Colorado","New Mexico","Utah","Montana","Wyoming")

Demographics_NorthEast =  subset(Demographics, (Demographics$State %in% NorthEast))
Shooting_NorthEast =  subset(Shootings, (Shootings$State %in% NorthEast))
Shooting_NorthEast = colSums(Filter(is.numeric, Shooting_NorthEast))


Demographics_South = subset(Demographics, (Demographics$State %in% South))
Shooting_South =  subset(Shootings, (Shootings$State %in% South))
Shooting_South =  colSums(Filter(is.numeric, Shooting_South))

Demographics_Midwest = subset(Demographics, (Demographics$State %in% Midwest))
Shooting_Midwest =  subset(Shootings, (Shootings$State %in% Midwest))
Shooting_Midwest = colSums(Filter(is.numeric, Shooting_Midwest))

#Demographics_Mountain = subset(Demographics, (Demographics$State %in% Mountain))
#Shooting_Mountain =  subset(Shootings, (Shootings$State %in% Mountain))
#Shooting_Mountain = colSums(Filter(is.numeric, Shooting_Mountain))

Demographics_West = subset(Demographics, (Demographics$State %in% West))
Shooting_West =  subset(Shootings, (Shootings$State %in% West))
Shooting_West = colSums(Filter(is.numeric, Shooting_West))

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

#Demographics_Mountain = Demographics_Mountain %>%
 # summarise(TotalPop = sum(TotalState),
  #          Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
   #         White = (sum(TotalState*(White)))/TotalPop,
    #        Black = (sum(TotalState*(Black)))/TotalPop,
     #       Native = (sum(TotalState*(Native)))/TotalPop,
      #      Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
#Demographics_Mountain$TotalPop = NULL

Demographics_West = Demographics_West %>%
  summarise(TotalPop = sum(TotalState),
            Hispanic = (sum(TotalState*(Hispanic)))/TotalPop,
            White = (sum(TotalState*(White)))/TotalPop,
            Black = (sum(TotalState*(Black)))/TotalPop,
            Native = (sum(TotalState*(Native)))/TotalPop,
            Asian_Pacific = (sum(TotalState*(Asian_Pacific)))/TotalPop)
Demographics_West$TotalPop = NULL
#CSVs for NORTHEAST
write.csv(Shooting_NorthEast, file = "csvs/Northeeast_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_NorthEast, file = "csvs/RacesOfNortheast.csv",row.names=FALSE)

#CSVs for South
write.csv(Shooting_South, file = "csvs/South_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_South, file = "csvsRacesOfSouth.csv",row.names=FALSE)

#CSVs for Midwest
write.csv(Shooting_Midwest, file = "csvs/Midwest_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_Midwest, file = "csvs/RacesOfMidwest.csv",row.names=FALSE)

#CSVs for Mountain
#write.csv(Shooting_Mountain, file = "Mountain_RacesOfVictims.csv",row.names=FALSE)
#write.csv(Demographics_Mountain, file = "RacesOfMountain.csv",row.names=FALSE)

#CSVs for West
write.csv(Shooting_West, file = "csvs/West_RacesOfVictims.csv",row.names=FALSE)
write.csv(Demographics_West, file = "csvs/RacesOfWest.csv",row.names=FALSE)

