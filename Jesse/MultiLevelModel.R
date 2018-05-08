library(dplyr)

setwd("/Users/sibela/Dropbox (Brown)/DSI_Courses/Semester2/Data2020/Final")
## PREPROCESSING
# Read Data
killings = read.csv("police_killings_cleaned.csv", stringsAsFactors=F)
census = read.csv("acs2015_census_tract_data.csv", stringsAsFactors=F)

# Get State and County Level info from Census data
census = read.csv("acs2015_census_tract_data.csv")
census = na.omit(census)

CountyInfo = census %>%
  group_by(County) %>% 
  summarise(num = n(),
            TotalCounty = sum(TotalPop),
            TotalMen = sum(Men),
            TotalWomen = sum(Women),
            CountyUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalCounty,
            CountyPoverty = (sum(TotalPop*(Poverty*.01)))/TotalCounty,
            CountyHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalCounty,
            CountyBlack = (sum(TotalPop*(Black*.01)))/TotalCounty,
            CountyWhite = (sum(TotalPop*(White*.01)))/TotalCounty)            

StateInfo = census %>%
  group_by(State) %>% 
  summarise(num = n(),
            TotalState = sum(TotalPop),
            TotalMen = sum(Men),
            TotalWomen = sum(Women),
            StateUnemployment =  (sum(TotalPop*(Unemployment*.01)))/TotalState,
            StatePoverty = (sum(TotalPop*(Poverty*.01)))/TotalState,
            FracHispanic = (sum(TotalPop*(Hispanic*.01)))/TotalState,
            FracBlack = (sum(TotalPop*(Black*.01)))/TotalState,
            FracWhite = (sum(TotalPop*(White*.01)))/TotalState)


merged1<- merge(census, CountyInfo , by.x="County", by.y="County")
merged2Census<- merge(merged1, StateInfo , by.x="State", by.y="State")

# Read merged file
merged <- merge(killings, merged2Census, by.x="geo_id", by.y="CensusTract")
#merged = read.csv("Merged.csv")
colnames(merged) = tolower(colnames(merged))

# Omit NaN
merged = na.omit(merged)
merged = merged[ ! merged$raceethnicity %in% "Unknown", ]

# Convert "men","women","employed" columns to percentage. 
merged$menpercent = (merged$men / merged$totalpop)*100
merged$womenpercent = (merged$women / merged$totalpop)*100
merged$employedpercent = (merged$employed / merged$totalpop)*100

# Drop unnecessary columns
drop = c("x.1","geo_id","x","streetaddress","latitude", "longitude","state_fp","county_fp","tract_ce","county_id","lawenforcementagency",
         "incomeerr", "incomepercaperr","income", "incomepercap", "year", "namelsad", "state.1", 
         "month", "day", "totalpop","men", "women","employed")

merged1 = merged[,!(names(merged) %in% drop)] 
merged1 = merged1[,-22]
colnames(merged1)

#write.csv(merged1, file = "MergedFinal.csv")

merged = read.csv("MergedData.csv")
dim(merged)

# Drop unnecessary columns
drop = c("x.1","geo_id","x","streetaddress","latitude", "longitude","state_fp","county_fp","tract_ce","county_id","lawenforcementagency",
         "incomeerr", "incomepercaperr","income", "incomepercap", "year", "namelsad", "state.1", 
         "month", "day", "totalpop","men", "women","employed")
merged1 = merged[,!(names(merged) %in% drop)] 

# Convert race into binary variable
merged1$raceethnicity = as.character(merged1$raceethnicity)
merged1$raceethnicity[which(merged1$raceethnicity == "Black")] = "1"
merged1$raceethnicity[which(merged1$raceethnicity == "White")] = "0"
merged1$raceethnicity[which(merged1$raceethnicity == "Hispanic/Latino")] = "0"
merged1$raceethnicity[which(merged1$raceethnicity == "Asian/Pacific Islander")] = "0"
merged1$raceethnicity[which(merged1$raceethnicity == "Hispanic/Latino")] = "0"
merged1$raceethnicity[which(merged1$raceethnicity == "Native American")] = "0"
merged1$raceethnicity <- as.factor(merged1$raceethnicity)

# Create Multilevel Model
M1 = glmer(raceethnicity ~ age +  (1|County), family = binomial("logit"), data = merged1)
summary(M1)

M2 = glmer(raceethnicity ~ age + gender +  (1|County), family = binomial("logit"), data = merged1)
summary(M2)

anova(M1,M2)

M3 = glmer(raceethnicity ~ age + gender + cause + (1|County), family = binomial("logit"), data = merged1)
summary(M3)

anova(M2,M3)

M4 = glmer(raceethnicity ~ age + gender + armed + (1|County), family = binomial("logit"), data = merged1)
summary(M4)

anova(M3,M4)












