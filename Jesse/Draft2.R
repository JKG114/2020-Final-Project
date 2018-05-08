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
#factor variables
total$raceethnicity = factor(total$raceethnicity)
total$armed = factor(total$armed)
total$gender = factor(total$gender)
#correct the age issue
total$age = total$age + 15
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

#consider varying slope varying intercept model with intercept varying for county
#and slope varying for state
#datacountystate <- merge(datacou)

#Variables to keep in multilevel models
keepsstate <-c("raceethnicity","age","armed", "gender", "TotalState" ,"StateIncomePerCap", 
          "StateUnemployment", "StatePoverty", "StateMen", "State")
keepscounty <-c("raceethnicity","age","armed", "gender", "TotalCounty" ,"CountyIncomePerCap", 
          "CountyUnemployment", "CountyPoverty", "CountyMen", "County")



#Create dataframe for multilevelmodels with state
datastateuse <- datastate[ , (names(datastate) %in% keepsstate)]
datastateuse$Minority <- ifelse(datastateuse$raceethnicity != "White", 1, 0)
datastateuse$State <- factor(datastateuse$State)
datastateuse$armed <- ifelse(datastateuse$armed != "No", 0, 1)
datastateuse$gender <- ifelse(datastateuse$gender != "Male", 1, 0)

#create dataframe for multilevelmodels with county
datacountyuse <- datacounty[ , (names(datacounty) %in% keepscounty)]
datacountyuse$Minority <- ifelse(datacountyuse$raceethnicity != "White", 1, 0)
datacountyuse$County <- factor(datacountyuse$County)
datacountyuse$armed <- ifelse(datacountyuse$armed != "No", 0, 1)
#datacountyuse$gender <- ifelse(datacountyuse$gender != "Male", 1, 0)




InterceptStateModel <-glmer(Minority ~ age + gender + armed + (1|State),
                   family = binomial("logit"), data = datastateuse)

#unfortunately the model fails to converge for feautres other than age and 
#gender having varying coefficients
OneSlopeStateModel <-glmer(Minority ~ age + gender + armed +(1+age|State),
                   family = binomial("logit"), data = datastateuse)
TwoSlopeStateModel <-glmer(Minority ~ age + gender + armed +(1+age+gender|State),
                        family = binomial("logit"), data = datastateuse)

InterceptCountyModel <- glmer(Minority ~ age + gender + armed + (1|County),
                              family = binomial("logit"), data = datacountyuse)

#Unfortunately, we don't have enough observations to run a varying slope model for the county
#SlopeCountyModel <- glmer(Minority ~ age + gender + (1+age+gender|County),
#                             family = binomial("logit"), data = datacountyuse)

#Something to consider is a varying slope varying intercept model using county and state level predictors
#InterceptCountySlopeStateModel <- glmer(Minority ~ age + gender + (1|County) +(1+age+gender|State),
 #                             family = binomial("logit"), data = datacountyuse)

## Compare models
kable(anova(InterceptStateModel, OneSlopeStateModel,TwoSlopeStateModel), type = "pandoc", caption = "Table 1: Comparison of varying/intercept slope and varying intercept on State")
summary(InterceptStateModel)
summary(OneSlopeStateModel)
summary(TwoSlopeStateModel)
summary(InterceptCountyModel)
