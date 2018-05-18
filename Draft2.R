library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(dplyr)
library(lme4) 
library(nlme) 
library(knitr) 


#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
acs = read.csv("acs2015_census_tract_data.csv")
#acs <- na.omit(acs)
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total = total[ ! total$raceethnicity %in% "Unknown", ]

#state_pop = aggregate(TotalPop~State,acs,sum)
total = merge(total, state_pop, by = "State")


toytable = acs %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            TotalMen = sum(Men)/TotalState,
            TotalWomen = sum(Women)/TotalState,
            StateUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalState,
            StateIncomePerCap = sum(TotalPop*IncomePerCap)/TotalState,
            StatePoverty = (sum(TotalPop*(Poverty*.01)))/TotalState,
            FracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            FracBlack = (sum(TotalPop*(Black*.01)))/TotalState,
            FracWhite = (sum(TotalPop*(White*.01)))/TotalState)

toycounty = acs %>%
  group_by(County) %>% 
  summarise(TotalCounty = sum(TotalPop),
            TotalMen = sum(Men)/TotalCounty,
            TotalWomen = sum(Women)/TotalCounty,
            StateUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalCounty,
            StateIncomePerCap = sum(TotalPop*IncomePerCap)/TotalCounty,
            StatePoverty = (sum(TotalPop*(Poverty*.01)))/TotalCounty,
            FracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalCounty,
            FracBlack = (sum(TotalPop*(Black*.01)))/TotalCounty,
            FracWhite = (sum(TotalPop*(White*.01)))/TotalCounty)


total_state = merge(toytable,total,by="State")
total = merge(toycounty,total, by = "County")
write.csv(total, file = "MergedData.csv")

#Convert "men","women","employed" columns to percentage. 
#total$menpercent = (total$men / merged$totalpop)*100
#total$womenpercent = (total$women / merged$totalpop)*100
#total$employedpercent = (total$employed / total$totalpop)*100

# Drop unnecessary columns
drop = c("x.1","geo_id","x","streetaddress","latitude", "longitude","state_fp","county_fp","tract_ce","county_id","lawenforcementagency",
         "incomeerr", "incomepercaperr","income", "incomepercap", "year", "namelsad", "state.1", 
         "month", "day", "totalpop","men", "women","employed")
total = total[,!(names(total) %in% drop)] 

total$armed <- as.factor(total$armed)
total$gender <- as.factor(total$gender)
total$raceethnicity <-as.factor(total$raceethnicity)

# Model both temperature and time
model.county = lmer(raceethnicity ~ age + armed + gender + (1+age+armed + gender|County),REML = FALSE, data=total)


model.0 = lmer(pain ~ 1|ID,REML = FALSE, data=datause) #Basic model with random intercept # Include time as a random effect


