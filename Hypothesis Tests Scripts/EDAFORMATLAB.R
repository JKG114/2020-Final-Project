library(ggplot2)
library(ggcorrplot)
library(corrplot)



setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
P = police[sapply(police, is.numeric)] 
P = na.omit(P)
total <- merge(acs,police,by="geo_id")
total <-na.omit(total)
total = total[ ! total$raceethnicity %in% "Unknown", ]


P$geo_id = NULL
P$latitude= NULL
P$longitude = NULL
P$tract_ce = NULL
P$county_fp = NULL
P$state_fp = NULL
P$county_id = NULL

M = cor(P)
corrplot(M,method="circle")
str(P)

M = total
M = M[sapply(M, is.numeric)] 

M$geo_id = NULL
M$latitude = NULL
M$longitude = NULL
M$name = NULL
M$County = NULL
M$State = NULL
M$streetaddress = NULL
M$namelsad = NULL
M$city = NULL
M$state_fp = NULL
M$county_bucket = NULL
M$county_fp = NULL
M$nat_bucket = NULL
M$IncomeErr = NULL
M$county_id = NULL
M$IncomePerCapErr = NULL
M$tract_ce=NULL
M$Income = NULL
M$IncomePerCap = NULL

  
N = cor(M)
corrplot(P,method="circle")
ggcorrplot(P, method = "circle")


df = na.omit(total)
rownames(df) <- 1:nrow(df)
stat = subset(df, select=c("geo_id", "raceethnicity", "Hispanic","White", "Black", "Native","Asian",
                           "Pacific"))


stat$raceethnicity <- as.character(stat$raceethnicity)
stat = stat[ ! stat$raceethnicity %in% "Unknown", ]
stat$raceethnicity[stat$raceethnicity== "Hispanic/Latino"] <- "Hispanic"
stat$raceethnicity[stat$raceethnicity== "Native American"] <- "Native"

stat$raceethnicity[stat$raceethnicity== "Asian/Pacific Islander"] <- "Asian_Pacific"
#stat$raceethnicity <- as.factor(stat$raceethnicity)
stat$Asian_Pacific = stat$Asian + stat$Pacific

Races = subset(stat, select = c("geo_id","Hispanic", "White", "Black", "Native", "Asian_Pacific"))
Races = Races[!duplicated(Races$geo_id),]
rownames(Races) <- 1:nrow(Races)

Shootings <- matrix(0, ncol = 6, nrow = 415)
Shootings <- data.frame(Shootings)
names(Shootings)[names(Shootings) == 'X1'] <- 'geo_id'
Shootings$geo_id=Races$geo_id
names(Shootings)[names(Shootings) == 'X2'] <- 'Hispanic'
names(Shootings)[names(Shootings) == 'X3'] <- 'White'
names(Shootings)[names(Shootings) == 'X4'] <- 'Black'
names(Shootings)[names(Shootings) == 'X5'] <- 'Native'
names(Shootings)[names(Shootings) == 'X6'] <- 'Asian_Pacific'


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
Races = Races/100

Shootings$geo_id = NULL

# Write CSV's of Shootings and Races dataframes to export to Matlab
write.csv(Shootings, file = "RacesOfVictims.csv",row.names=FALSE)
write.csv(Races, file = "RacesOfCounties.csv",row.names=FALSE)


                                      