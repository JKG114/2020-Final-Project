library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(lme4) 
library(nlme) 
library(knitr) 
library(dplyr)
library(MASS)
library(bestglm)
require(reshape2)
require(compiler)
require(parallel)
require(boot)
require(lattice)


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
police$age = police$age + 15
police = na.omit(police)
police = police[ ! police$raceethnicity %in% "Unknown", ]

acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'

#We merge the acs dataframe and police dataframe
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total$White = total$White/100
total$Black = total$Black/100
total$Hispanic = total$Hispanic/100
total$Pacific = total$Pacific/100
total$Asian = total$Asian/100
total$Native - total$Native/100


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
acs$AsianPacific = acs$Asian + acs$Pacific
state_levels = acs %>%
  group_by(State) %>% 
  summarise(TotalState = sum(TotalPop),
            #Total_State_Women = sum(Women)/TotalState,
            State_Men = sum(Men)/TotalState,
            State_Unemployment =  (sum(TotalPop*Unemployment*.01))/TotalState,
            State_IncomePerCap = sum(TotalPop*IncomePerCapErr)/TotalState,
            State_Poverty = (sum(TotalPop*Poverty*.01))/TotalState,
            State_Drive = (sum(TotalPop*Drive*.01))/TotalState,
            State_Child_Poverty = (sum(TotalPop*.01*ChildPoverty))/TotalState,
            State_Hispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            State_Black = (sum(TotalPop*(Black*.01)))/TotalState,
            State_White = (sum(TotalPop*(White*.01)))/TotalState,
            State_Asian_Pacific = (sum(TotalPop*(AsianPacific*.01)))/TotalState,
            State_Native = (sum(TotalPop*(Native*.01)))/TotalState)



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
totalNE$Region = "NE"
Demographics_NorthEast$Region = "NE"
totalNE = merge(totalNE, Demographics_NorthEast, by= "Region")

totalS = subset(total, (total$State %in% South))
totalS$Region = "South"
Demographics_South$Region = "South"
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
STATE = total$State
drop = c("Region", "State", "geo_id", "County","IncomeErr", "IncomePerCapErr","Native","Asian",
         "Employed", "PrivateWork","WorkAtHome","share_black", "share_hispanic","share_white",
         "p_income","h_income","county_income", "TotalState.x", "State_Men.x","State_Unemployment.x", 
         "State_IncomePerCap.x",  "State_Poverty.x","State_Hispanic.x","State_Black.x","State_White.x",
         "State_Drive.x","State_Child_Poverty.x", "TotalState.y","State_Men.y","State_Unemployment.y","State_IncomePerCap.y",  
         "State_Poverty.y", "State_Hispanic.y","State_Black.y","State_White.y","State_Drive.y","State_Child_Poverty.y", "cause")
total = total[,!(names(total) %in% drop)]


#This one is not clear so I leave it out of the above list (same goes for cause maybe..)
total$armed = NULL
total$pov = NULL
#Turn Gender into 1's and zeros
cols <- sapply(total, is.logical)
total[,cols] <- lapply(total[,cols], as.numeric)
#Okay Time for some feature selection:
#We will use logistic regression to figure out which features we should
#consider using. 
full <- glm(raceethnicity ~.,data = total, family = binomial())
#Degrees of Freedom: 420 Total (i.e. Null);  375 Residual
#Null Deviance:	    515.5 
#Residual Deviance: 318.1 	AIC: 410.1
#Not good but better than our scores multilevel

step <- stepAIC(full,direction = "both", trace = FALSE)
step$anova

#Final Selection Variables:
#Final Model:
 #raceethnicity ~ TotalPop + White + Black + Professional + Service + 
  #Office + Construction + Production + Carpool + OtherTransp + 
  #PublicWork + SelfEmployed + Unemployment + age + comp_income + 
  #nat_bucket + college + State_IncomePerCap + State_Hispanic + 
  #TotalRegion


forward <- stepAIC(full,direction = "forward", trace = FALSE)
forward$anova
#Somewhat dissapointingly gives the same variables back for final 
#model (in fact, if both can't be applied reverts to backward)!

backward <-stepAIC(full, direction = "backward", trace = FALSE)
backward$anova
#Final Selection Variables:
#Final Model:
  #raceethnicity ~ TotalPop + White + Black + Professional + Service + 
  #Office + Construction + Production + Carpool + OtherTransp + 
  #PublicWork + SelfEmployed + Unemployment + age + comp_income + 
  #nat_bucket + college + State_IncomePerCap + State_Hispanic + 
  #TotalRegion
#We now build a normal logistic model with some of the recommended variables. 
#A TRAIN TEST SPLIT
## 75% of the sample size
smp_size <- floor(0.75 * nrow(total))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(total)), size = smp_size)

train <- total[train_ind, ]
test <- total[-train_ind, ]

glm.logit = glm(raceethnicity ~ White + Black + Professional + Service + Office +
                  Construction + Production + Carpool + OtherTransp+ age  +
                  comp_income + nat_bucket + college +State_IncomePerCap + State_Hispanic + TotalRegion,
                family = binomial, data = train)

library(gridExtra)
library(pROC) 
p <- predict(glm.logit, newdata=test, type="response")
plot(roc(test$raceethnicity,p), legacy.axes = TRUE)
auc_logit =auc(roc(test$raceethnicity,p))
title(main = "ROC Logistic Regression", line = +3)

#The deviance residuals for the predictions on the trianed data)
#gg <- qplot(x = fitted(glm.logit), y = residuals(glm.logit)) +
  #geom_smooth(method = "glm", se = FALSE) +
 # geom_point(alpha = 0.3, size = 3) +
  #theme_bw()

#print(gg)


#test <- test %>%
 # mutate(test$raceethnicity = test$raceethnicity)
#test <- test %>%
 # mutate(predicted.prob = p)
#test <- test %>%
 # mutate(predicted = ifelse(predicted.prob >0.5, 1, 0))
#table(test$raceethnicity, test$predicted)
#table(test$raceethnicity)
#roc(test$raceethnicity, test$predicted.prob)
#png("1d.png", width = 400, height =400, res = 110)
#ggplot(test, aes(d = raceethnicity, m = predicted.prob)) +
 # geom_abline(slope = 1, intercept = 0) +
  #labs(x = "1 - Specificity", y = "Sensitivity")
#dev.off()

#ggplot(test$raceethnicity, p) +
 # labs(x = "1 - Specificity", y = "Sensitivity")

Xy=total
Xy$raceethnicity = NULL

Xy = cbind(Xy, total$raceethnicity)
Xy$gender = NULL
myglm <-bestglm(Xy,nvmax = 8)
Xy = Xy[,c("TotalPop", "White","Black","Professional","Unemployment",
              "Service","Construction","State_IncomePerCap","comp_income",
              "age","college","Office", "nat_bucket","IncomePerCap","State_Hispanic")]
Xy$black_killed = total$raceethnicity
#Xy$nonBlack = 1 - total$raceethnicity
myglm_logit <-bestglm(Xy,family = binomial(), nvmax = 5)


#Now that we have looked at a few different glm's to predict whether a person
#shot was black or not we build a multilevel model. We start by scaling the data where appropriate
total$Scale_State_IncomePerCap = scale(total$State_IncomePerCap)
#total$State_Hispanic = scale(total$State_Hispanic)
total$Scale_TotalPop = scale(total$TotalPop)
total$age_scale = scale(total$age)
total$Scale_TotalRegion = scale(total$TotalRegion)


#A TRAIN TEST SPLIT
## 75% of the sample size
smp_size <- floor(0.8 * nrow(total))
set.seed(123)
train_ind <- sample(seq_len(nrow(total)), size = smp_size)
total$State = STATE

train <- total[train_ind, ]
test <- total[-train_ind, ]

#Test with states that were includeded in the training data
test = test[test$State %in% unique(train$State),]

#STATE INTERCEPT
model.state.intercept = glmer(raceethnicity ~ Scale_TotalPop + age_scale + Professional+ nat_bucket+Black + White  +comp_income 
                              + (1|Scale_State_IncomePerCap),
                   family = binomial("logit"),REML = FALSE, data=train,
                   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

se1 <- sqrt(diag(vcov(model.state.intercept)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.state.intercept), 
              LL = fixef(model.state.intercept) - 1.96 * se1, UL = fixef(model.state.intercept) + 1.96 *se1))

#print(model.state.intercept, corr = FALSE)
predictions.state.intercept <- predict(model.state.intercept, test, type = "response")
roc_model.state.intercept <- roc(test$raceethnicity ~ predictions.state.intercept)
auc1 = auc(roc_model.state.intercept)
plot(roc_model.state.intercept)    
title(main = "ROC Predictions State Intercept Model", line = +3)




#We have a STATE slope model
model.state.slope = glmer(raceethnicity ~ Scale_TotalPop + age_scale+ Professional+ nat_bucket+Black + White  +comp_income
                         + (Black+age_scale + nat_bucket|Scale_State_IncomePerCap), 
                         family = binomial("logit"),REML = FALSE, data=train,
                         glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))



se2 <- sqrt(diag(vcov(model.state.slope)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.state.slope), 
              LL = fixef(model.state.slope) - 1.96 * se2, UL = fixef(model.state.slope) + 1.96 *se2))
print(model.state.slope, corr = FALSE)

predictions.state.slope <- predict(model.state.slope, test, type = "response")
roc_model.state.slope <- roc(test$raceethnicity ~ predictions.state.slope)
auc2 = auc(roc_model.state.slope)
plot(roc_model.state.slope)    
title(main = "ROC Predictions State Slope Model", line = +3)

# STATE SLOPE
model.state.slope.intercept = glmer(raceethnicity ~ Scale_TotalPop + age_scale+ Professional+ nat_bucket+Black + White + gender +comp_income
                          + (1+Black+age_scale + nat_bucket|Scale_State_IncomePerCap), 
                          family = binomial("logit"),REML = FALSE, data=train,
                          glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

se3 <- sqrt(diag(vcov(model.state.slope.intercept)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.state.slope.intercept), 
              LL = fixef(model.state.slope.intercept) - 1.96 * se3, UL = fixef(model.state.slope.intercept) + 1.96 *se3))
print(model.state.slope.intercept, corr = FALSE)
predictions.state.slope.intercept <- predict(model.state.slope.intercept, test, type = "response")
roc_model.state.slope.intercept <- roc(test$raceethnicity ~ predictions.state.slope.intercept)
auc3 = auc(roc_model.state.slope.intercept)
plot(roc_model.state.slope.intercept)    
title(main = "ROC Predictions state slope intercept Model" ,line = +3)

anova(model.state.intercept,model.state.slope,model.state.slope.intercept)   

#REGION INTERCEPT
model.region.intercept = glmer(raceethnicity ~ Scale_TotalPop+age_scale+ Professional+ nat_bucket+Black + White + gender +comp_income
                                    + Scale_State_IncomePerCap+ State_Hispanic+ (1|Scale_TotalRegion), 
                                    family = binomial("logit"),REML = FALSE, data=train,
                                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

se4 <- sqrt(diag(vcov(model.region.intercept)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.region.intercept), 
              LL = fixef(model.region.intercept) - 1.96 * se4, UL = fixef(model.region.intercept) + 1.96 *se4))
print(model.region.intercept, corr = FALSE)

predictions.model.region.intercept <- predict(model.region.intercept, test, type = "response")
roc_model.region.intercept <- roc(test$raceethnicity ~ predictions.model.region.intercept)
auc4= auc(roc_model.region.intercept)
plot(roc_model.region.intercept)    
title(main = "ROC Predictions Region Intercept Model", line = +3)




#REGION VARYING SLOPE

model.region.slope = glmer(raceethnicity ~ Scale_TotalPop+age_scale+ Professional+ nat_bucket+Black + White + gender +comp_income
                               + Scale_State_IncomePerCap+ State_Hispanic+ (Scale_State_IncomePerCap+ comp_income+age_scale + Black|Scale_TotalRegion), 
                               family = binomial("logit"),REML = FALSE, data=train,
                               glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

se5 <- sqrt(diag(vcov(model.region.slope)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.region.slope), 
              LL = fixef(model.region.slope) - 1.96 * se5, UL = fixef(model.region.slope) + 1.96 *se5))
print(model.region.slope, corr = FALSE)
prediction.model.region.slope <- predict(model.region.slope, test, type = "response")
roc_model.region.slope <- roc(test$raceethnicity ~ prediction.model.region.slope)
auc5= auc(roc_model.region.slope)
plot(roc_model.region.slope)    
title(main = "ROC Predictions Region Slope Model", line = +3)

#REGION VARYING SLOPE INTERCEPT

model.region.slope.intercept = glmer(raceethnicity ~Scale_TotalPop + age_scale+ Professional+ nat_bucket+Black + White + gender +comp_income
                           + Scale_State_IncomePerCap+ State_Hispanic+ (1 +Scale_State_IncomePerCap+ comp_income+age_scale + Black|Scale_TotalRegion), 
                           family = binomial("logit"),REML = FALSE, data=train,
                           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

se6 <- sqrt(diag(vcov(model.region.slope.intercept)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(model.region.slope.intercept), 
              LL = fixef(model.region.slope.intercept) - 1.96 * se6, UL = fixef(model.region.slope.intercept) + 1.96 *se6))
print(model.region.slope.intercept, corr = FALSE)
predictions.region.region.slope.intercept <- predict(model.region.slope.intercept, test, type = "response")
roc_region.slope.intercept <- roc(test$raceethnicity ~ predictions.region.region.slope.intercept)
auc6= auc(roc_region.slope.intercept)
plot(roc_region.slope.intercept)    
title(main = "ROC Predictions Region Slope Intercept Model", line = +3)


anova(model.state.intercept,model.state.slope,model.state.slope.intercept, 
      model.region.intercept,model.region.slope ,model.region.slope.intercept)   

