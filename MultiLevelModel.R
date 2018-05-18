library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 


#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
#Get the Police killing data ready (cleaned and remove columns)
police = read.csv("police_killings_cleaned.csv")
drop = c("X","name","month","day","year","streetaddress", "city","latitude","longitude",
         "state_fp","county_fp","tract_ce","county_id","namelsad","lawenforcementagency",
         "pop","state")
police = police[,!(names(police) %in% drop)]
police$age = police$age + 15
police = police[ ! police$raceethnicity %in% "Unknown", ]

police$raceethnicity <- as.character(police$raceethnicity)
police$raceethnicity[police$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
police$raceethnicity[police$raceethnicity== "Native American"] <- "Native"
police$raceethnicity[police$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"

# Convert race into binary variable
police$raceethnicity[which(police$raceethnicity == "Black")] = "1"
police$raceethnicity[which(police$raceethnicity == "White")] = "0"
police$raceethnicity[which(police$raceethnicity == "Hispanic/Latino")] = "0"
police$raceethnicity[which(police$raceethnicity == "Asian/Pacific Islander")] = "0"
police$raceethnicity[which(police$raceethnicity == "Hispanic/Latino")] = "0"
police$raceethnicity[which(police$raceethnicity == "Native American")] = "0"

#Get the census data ready (including ready to merge with police)
acs = read.csv("acs2015_census_tract_data.csv")
acs <- na.omit(acs)
acs$Asian_Pacific = acs$Asian + acs$Pacific

names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
#Merge The Data
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total$Men = total$Men/total$TotalPop
total$Women = NULL
#state_pop = aggregate(TotalPop~State,acs,sum)
#rownames(total) <- 1:nrow(total)

#Get the state level Aggregate scores for 
#Notice we don't do Native or Asian, since these features correspond to very few killings
#and will simply increase the "dependency" between our races
state_levels = acs %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            Total_State_Men = sum(Men)/TotalState,
            #Total_State_Women = sum(Women)/TotalState,
            StateUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalState,
            StateIncomePerCap = sum(TotalPop*IncomePerCap)/TotalState,
            StatePoverty = (sum(TotalPop*(Poverty*.01)))/TotalState,
            State_Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            State_Black = (sum(TotalPop*(Black*.01)))/TotalState,
            State_White = (sum(TotalPop*(White*.01)))/TotalState)

county_levels = acs %>%
  group_by(County) %>% 
  summarise(TotalCounty = sum(TotalPop),
            Total_County_Men = sum(Men)/TotalCounty,
            #Total_County_Women = sum(Women)/TotalCounty,
            CountyUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalCounty,
            CountyIncomePerCap = sum(TotalPop*IncomePerCap)/TotalCounty,
            CountyPoverty = (sum(TotalPop*(Poverty*.01)))/TotalCounty,
            County_Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalCounty,
            County_Black = (sum(TotalPop*(Black*.01)))/TotalCounty,
            County_White = (sum(TotalPop*(White*.01)))/TotalCounty)


total_state = merge(state_levels,total,by="State")
total = merge(county_levels,total, by = "County")
#write.csv(total, file = "MergedData.csv")

#Convert "men","women","employed" columns to percentage. 
#total$menpercent = (total$men / merged$totalpop)*100
#total$womenpercent = (total$women / merged$totalpop)*100
#total$employedpercent = (total$employed / total$totalpop)*100

# Drop unnecessary columns
drop = c("x.1","geo_id","x","streetaddress","latitude", "longitude","state_fp","county_fp","tract_ce","county_id","lawenforcementagency",
         "incomeerr", "incomepercaperr","income", "incomepercap", "year", "namelsad", "state", 
         "month", "day", "totalpop","men", "women","employed")
total = total[,!(names(total) %in% drop)] 

total$armed <- as.factor(total$armed)
total$gender <- as.factor(total$gender)
total$raceethnicity <-as.factor(total$raceethnicity)

# Model both temperature and time
model.county = lmer(raceethnicity ~ age + armed + gender + (1|State),REML = FALSE, data=total)


model.0 = lmer(raceethnicity ~ 1|State,REML = FALSE, data=total) #Basic model with random intercept # Include time as a random effect


