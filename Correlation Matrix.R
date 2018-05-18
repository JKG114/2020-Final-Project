acs = read.csv("acs2015_census_tract_data.csv")
police = read.csv("police_killings_cleaned.csv")
police$age = police$age + 15
police = police[ ! police$raceethnicity %in% "Unknown", ]
# Convert race into binary variable

police$raceethnicity <- as.character(police$raceethnicity)
police$raceethnicity[which(police$raceethnicity == "Black")] = "1"
police$raceethnicity[which(police$raceethnicity == "White")] = "0"
police$raceethnicity[which(police$raceethnicity == "Hispanic/Latino")] = "0"
police$raceethnicity[which(police$raceethnicity == "Asian/Pacific Islander")] = "0"
police$raceethnicity[which(police$raceethnicity == "Hispanic/Latino")] = "0"
police$raceethnicity[which(police$raceethnicity == "Native American")] = "0"
police$raceethnicity <- as.numeric(police$raceethnicity)

names(acs)[names(acs) == 'CensusTract'] <- 'geo_id'
#Merge The Data
data <- merge(acs,police,by="geo_id")



colnames(data)
vars <- c("age", "p_income", "county_income", "comp_income", "county_bucket", 
          "nat_bucket", "college", "TotalPop", "Men", "Women", "Hispanic", "White", 
          "Black", "Native", "Asian", "Pacific", "Citizen", "Income", "IncomeErr", 
          "IncomePerCap", "IncomePerCapErr", "Poverty",
          "Professional", "Service", "Office", "Construction","Production", 
          "Drive", "Carpool", "Transit", "Walk", "OtherTransp", "WorkAtHome", "MeanCommute", 
          "Employed", "PrivateWork", "PublicWork", "SelfEmployed", "FamilyWork", 
          "Unemployment", "raceethnicity") 
newdata <- data[vars]
newdata <-na.omit(newdata)
indx <- sapply(newdata, is.factor)
newdata[indx] <- lapply(newdata[indx], function(x) as.numeric(as.character(x)))
newdata$raceethnicity <- as.numeric(as.character(newdata$raceethnicity))

library(corrplot)
png(height=1200, width=1500, pointsize=15, file="overlap.png")
corrplot(cor(newdata), method="square", type = "upper")
