library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 


#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
#Get the Police killing data ready (cleaned and remove columns)
#police = read.csv("police_killings_cleaned.csv")
#drop = c("X","name","month","day","year","streetaddress", "city","latitude","longitude",
  #       "state_fp","county_fp","tract_ce","county_id","namelsad","lawenforcementagency",
 #        "pop","state")
#police = police[,!(names(police) %in% drop)]
#police$age = police$age + 15
#police = na.omit(police)
#police = police[ ! police$raceethnicity %in% "Unknown", ]

police = read.csv("police_killings_cleaned.csv")
police$X = NULL
police = na.omit(police)
police = police[ ! police$raceethnicity %in% "Unknown", ]

acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'

#We merge the acs dataframe and police dataframe
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)


total$raceethnicity <- as.character(total$raceethnicity)
total$raceethnicity[total$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
total$raceethnicity[total$raceethnicity== "Native American"] <- "Native"
total$raceethnicity[total$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"

# Convert race into binary variable
total$raceethnicity[which(total$raceethnicity == "Black")] = "1"
total$raceethnicity[which(total$raceethnicity == "White")] = "0"
total$raceethnicity[which(total$raceethnicity == "Hispanic")] = "0"
total$raceethnicity[which(total$raceethnicity == "Asian_Pacific")] = "0"
total$raceethnicity[which(total$raceethnicity == "Hispanic/Latino")] = "0"
total$raceethnicity[which(total$raceethnicity == "Native")] = "0"

total$raceethnicity <- as.numeric(as.character(total$raceethnicity))

#Get the census data ready (including ready to merge with police)
acs = read.csv("acs2015_census_tract_data.csv")
acs <- na.omit(acs)
acs$Asian_Pacific = acs$Asian + acs$Pacific

names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
#get rid of columns that are redundant after merge

total$Men = total$Men/total$TotalPop
total$Women = NULL
#state_pop = aggregate(TotalPop~State,acs,sum)
#rownames(total) <- 1:nrow(total)

#Get the state level Aggregate Data
#Notice we don't do Native or Asian, since these features correspond to very few killings
#and will simply increase the "dependency" between our races
state_levels = acs %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            State_Men = sum(Men)/TotalState,
            #Total_State_Women = sum(Women)/TotalState,
            State_Unemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalState,
            State_IncomePerCap = sum(TotalPop*IncomePerCap)/TotalState,
            State_Poverty = (sum(TotalPop*(Poverty*.01)))/TotalState,
            State_Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            State_Black = (sum(TotalPop*(Black*.01)))/TotalState,
            State_White = (sum(TotalPop*(White*.01)))/TotalState,
            State_Drive = (sum(TotalPop*(Drive*.01)))/TotalState,
            State_Child_Poverty = (sum(TotalPop*(ChildPoverty*.01)))/TotalState)

#I will now subset the states into regions Northeast, South, West, Midwest (I had a fifth, Mountain,
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


Demographics_NorthEast =  subset(state_levels, (state_levels$State %in% NorthEast))

Demographics_South = subset(state_levels, (state_levels$State %in% South))

Demographics_Midwest = subset(state_levels, (state_levels$State %in% Midwest))

Demographics_West = subset(state_levels, (state_levels$State %in% West))


#For Each Demographic Region We aggregate the demographic data
Demographics_NorthEast = Demographics_NorthEast %>%
  summarise(TotalRegion = sum(TotalState),
            Region_Men = sum(TotalState*State_Men)/TotalRegion,
            Region_Unemployment =  (sum(TotalState*State_Unemployment))/TotalRegion,
            Region_IncomePerCap = sum(TotalState*State_IncomePerCap)/TotalRegion,
            Region_Poverty = (sum(TotalState*State_Poverty))/TotalRegion,
            Region_Hispanic = (sum(TotalState*State_Hispanic))/TotalRegion,
            Region_Black = (sum(TotalState*State_Black))/TotalRegion,
            Region_White = (sum(TotalState*State_White))/TotalRegion,
            Region_Drive = (sum(TotalState*State_Drive))/TotalRegion,
            Region_Child_Poverty = (sum(TotalState*State_Child_Poverty))/TotalRegion)


Demographics_Midwest = Demographics_Midwest %>%
  summarise(TotalRegion = sum(TotalState),
            Region_Men = sum(TotalState*State_Men)/TotalRegion,
            Region_Unemployment =  (sum(TotalState*State_Unemployment))/TotalRegion,
            Region_IncomePerCap = sum(TotalState*State_IncomePerCap)/TotalRegion,
            Region_Poverty = (sum(TotalState*State_Poverty))/TotalRegion,
            Region_Hispanic = (sum(TotalState*State_Hispanic))/TotalRegion,
            Region_Black = (sum(TotalState*State_Black))/TotalRegion,
            Region_White = (sum(TotalState*State_White))/TotalRegion,
            Region_Drive = (sum(TotalState*State_Drive))/TotalRegion,
            Region_Child_Poverty = (sum(TotalState*State_Child_Poverty))/TotalRegion)

Demographics_South = Demographics_South %>%
  summarise(TotalRegion = sum(TotalState),
            Region_Men = sum(TotalState*State_Men)/TotalRegion,
            Region_Unemployment =  (sum(TotalState*State_Unemployment))/TotalRegion,
            Region_IncomePerCap = sum(TotalState*State_IncomePerCap)/TotalRegion,
            Region_Poverty = (sum(TotalState*State_Poverty))/TotalRegion,
            Region_Hispanic = (sum(TotalState*State_Hispanic))/TotalRegion,
            Region_Black = (sum(TotalState*State_Black))/TotalRegion,
            Region_White = (sum(TotalState*State_White))/TotalRegion,
            Region_Drive = (sum(TotalState*State_Drive))/TotalRegion,
            Region_Child_Poverty = (sum(TotalState*State_Child_Poverty))/TotalRegion)


Demographics_West = Demographics_West %>%
  summarise(TotalRegion = sum(TotalState),
            Region_Men = sum(TotalState*State_Men)/TotalRegion,
            Region_Unemployment =  (sum(TotalState*State_Unemployment))/TotalRegion,
            Region_IncomePerCap = sum(TotalState*State_IncomePerCap)/TotalRegion,
            Region_Poverty = (sum(TotalState*State_Poverty))/TotalRegion,
            Region_Hispanic = (sum(TotalState*State_Hispanic))/TotalRegion,
            Region_Black = (sum(TotalState*State_Black))/TotalRegion,
            Region_White = (sum(TotalState*State_White))/TotalRegion,
            Region_Drive = (sum(TotalState*State_Drive))/TotalRegion,
            Region_Child_Poverty = (sum(TotalState*State_Child_Poverty))/TotalRegion)

drop = c("name","month","day","year","streetaddress", "city","latitude","longitude",
       "state_fp","county_fp","tract_ce","county_id","namelsad","lawenforcementagency",
        "pop","state")

#Merge Regional Data On
total = total[,!(names(total) %in% drop)]
total = merge(total, state_levels, by= "State")

totalNE = subset(total, (total$State %in% NorthEast))
totalNE$Region = "Mid"
Demographics_NorthEast$Region = "Mid"
totalNE = merge(totalNE, Demographics_NorthEast, by= "Region")

totalS = subset(total, (total$State %in% South))
totalS$Region = "Mid"
Demographics_South$Region = "Mid"
totalS = merge(totalS, Demographics_South, by= "Region")

totalMid = subset(total, (total$State %in% Midwest))
totalMid$Region = "Mid"
Demographics_Midwest$Region = "Mid"
totalMid = merge(totalMid, Demographics_Midwest, by= "Region")

totalW = subset(total, (total$State %in% West))
totalW$Region = "West"
Demographics_West$Region = "West"
totalW = merge(totalW, Demographics_West, by= "Region")
total = rbind(totalNE,totalMid,totalS,totalW)
#We Drop colnames that are useless
drop = 
