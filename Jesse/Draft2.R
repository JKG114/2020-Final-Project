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
acs <- na.omit(acs)
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total = total[ ! total$raceethnicity %in% "Unknown", ]
total$raceethnicity = factor(total$raceethnicity)
total$armed = factor(total$armed)
total$gender = factor(total$gender)
#state_pop = aggregate(TotalPop~State,acs,sum)
#total = merge(total, state_pop, by = "State")


toytableState = acs %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            StateMen = sum(Men)/TotalState,
            #TotalWomenState = sum(Women),
            StateUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalState,
            StateIncomePerCap = sum(TotalPop*IncomePerCap)/TotalState,
            StatePoverty = (sum(TotalPop*(Poverty*.01)))/TotalState,
            StateFracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            StateFracBlack = (sum(TotalPop*(Black*.01)))/TotalState,
            StateFracWhite = (sum(TotalPop*(White*.01)))/TotalState)


toytableCounty = acs %>%
  group_by(County) %>% 
  summarise(TotalCounty = sum(TotalPop),
            CountyMen = sum(Men)/TotalCounty,
            #TotalWomen = sum(Women),
            CountyUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalCounty,
            CountyIncomePerCap = sum(TotalPop*IncomePerCap)/TotalCounty,
            CountyPoverty = (sum(TotalPop*(Poverty*.01)))/TotalCounty,
            CountyFracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalCounty,
            CountyFracBlack = (sum(TotalPop*(Black*.01)))/TotalCounty,
            CountyFracWhite = (sum(TotalPop*(White*.01)))/TotalCounty)
datastate <- merge(total,toytableState,by="State")
datacounty <- merge(total,toytableCounty,by ="County")

keeps <-c("raceethnicity","age","armed", "gender", "TotalState" ,"StateIncomePerCap", 
          "StateUnemployment", "StatePoverty", "StateMen", "State")
datastateuse <- datastate[ , (names(datastate) %in% keeps)]
datastateuse$Minority <- ifelse(datastateuse$raceethnicity != "White", 1, 0)
datastateuse$State= factor(datastateuse$State)

StateModel <-glmer(Minority ~ age + armed + gender + (1|"State"),family = binomial("logit"), data = datastateuse)

