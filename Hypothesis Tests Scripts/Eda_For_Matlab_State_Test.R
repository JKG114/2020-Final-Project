library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 


setwd('Users/stuartgeman/Desktop/data2020/Final Project')
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
    Shootings$Hispanic[index] <- Shootings$Hispanic[index] + (1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "White"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$White[index] <- Shootings$White[index] + (1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Black"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Black[index] <- Shootings$Black[index] + (1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Native"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Native[index] <- Shootings$Native[index] + (1/state_killed$freq.loc[i])
  }
  if(state_killed$raceethnicity[i] == "Asian_Pacific"){
    index <- Shootings$State == state_killed$State[i]
    Shootings$Asian_Pacific[index] <- Shootings$Asian_Pacific[index] + (1/state_killed$freq.loc[i])
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
            FracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            FracBlack = (sum(TotalPop*(Black*.01)))/TotalState,
            FracWhite = (sum(TotalPop*(White*.01)))/TotalState,
            FracNative = (sum(TotalPop*(Native*.01)))/TotalState,
            TotalAsian = sum(TotalPop*(Asian*.01)),
            TotalPacific = sum(TotalPop*(Pacific*.01)))

#Calculate the fraction of each state that's Asian/Pacific
Demographics$Asian_Pacific = (Demographics$TotalAsian + 
                                Demographics$TotalPacific)/Demographics$TotalState
#Get Rid of the Obselete columns
Demographics$TotalAsian = NULL
Demographics$TotalPacific = NULL
Demographics$TotalState = NULL

#Want to add a new column, NumKilled, with number killed in Each State
NumKilled = unique(subset(state_killed, select=c("freq.loc", "State")))
Demographics = merge(Demographics, NumKilled, by = "State")

#We now have the two dataframes we wanted