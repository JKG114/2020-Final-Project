library(ggplot2)
library(ggcorrplot)
library(corrplot)



#setwd('Users/stuartgeman/Desktop/data2020/Final Project')
police = read.csv("police_killings_cleaned.csv")
police$X = NULL
#Remove shootings where race of victim is unkown
police = police[ ! police$raceethnicity %in% "Unknown", ]

sum(police$raceethnicity =="Unknown")

acs = read.csv("acs2015_census_tract_data.csv")
names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
P = police[sapply(police, is.numeric)] 
c = acs[sapply(aes, is.numeric)]
P = na.omit(P)
c = na.omit(acs)
total <- merge(police,acs ,by="geo_id")

P$geo_id = NULL
P$latitude= NULL
P$longitude = NULL
P$tract_ce = NULL
P$county_fp = NULL
P$state_fp = NULL
P$county_id = NULL
P$year = NULL
police = read.csv("police_killings_cleaned.csv")


M = cor(P)
corrplot(M,method="circle")
ggcorrplot(M, method = "circle")
total <-merge(aes, police, by = "geo_id")

library(ggplot2)
police$age = police$age +15

police$raceethnicity <- as.character(police$raceethnicity)
police$gender = as.character(police$gender)
police$cause = as.character(police$cause)
police$armed = as.character(police$armed)
ggplot(police, aes(x = raceethnicity,fill = gender)) +geom_bar()
ggplot(police, aes(x = age, fill = raceethnicity)) + geom_bar()
ggplot(police, aes(x = cause, fill= raceethnicity)) + geom_bar()
ggplot(police, aes(x = cause, fill= armed)) + geom_bar()
ggplot(police, aes(x = raceethnicity, fill = armed)) + geom_bar()
police$unarmed = ifelse(police$armed == "No", "No","Yes")
ggplot(police, aes(x = raceethnicity, fill = unarmed)) + geom_bar()
ggplot(police, aes(x = raceethnicity, fill = county_bucket)) + geom_bar()

ggplot(police, aes(x = raceethnicity, fill = county_bucket)) + geom_bar()

ggplot(police, aes(x = raceethnicity, ))
barplot(prop.table(table(victims))) 
gender <- as.character(police$gender)

barplot(prop.table(table(gender)))

armed <- as.character(police$armed)

barplot(prop.table(table(armed)))

cause <- as.character(police$cause)

barplot(prop.table(table(cause)))
barplot(prop.table(table(police$age)))
barplot(prop.table(table(police$county_bucket, main = "Killings Across Income Level")))

Public_other_transit_wealth <- ggplot(police, aes(x = share_white, y = h_income))

white_poverty_graph <- ggplot(acs, aes(x = White, y = Unemployment))
white_poverty_graph + geom_line(aes(color = White))

black_h_inome_graph <- ggplot(police, aes(x = share_black, y = h_income))
black_h_inome_graph + geom_line(aes(color = raceethnicity))
