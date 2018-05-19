library(arm)
library(sjPlot)
library(sjmisc)
library(lme4)
png("4a.png", width = 600, height = 450, res = 100)
par(mai = c(0.8, 0.8, 0.1, 0.1))
plot(coef(fit.sub.4a)$county[,1], type = "l", lwd = 2, col = "pink",
         ylab = "County-level Intercepts", xlab = "Counties in the Subset")
 tmp <- rownames(coef(fit.full.4a)$county) %in%
  rownames(coef(fit.sub.4a)$county)
lines(coef(fit.full.4a)$county[tmp,1], lty = 2, lwd = 2, col = "yellowgreen")
 legend("bottomleft", c("Subset", "Entire"),
          49 lty = 1:2,
          50 col = c("pink", "yellowgreen"),
          51 lwd = 2)
dev.off()


coef(model.state.intercept)$State_IncomePerCap[,1]

gg <- ggplot(total, aes(x = State_IncomePerCap, y = raceethnicity, group = State_IncomePerCap)) +
 # geom_line(aes(y = PooledPredictions), color = "darkgrey") +
  #geom_line(aes(y = VaryingInterceptPredictions), color = "blue") +
  #geom_line(aes(y = VaryingSlopePredictions), color = "red") +
  #geom_line(aes(y = InteractionPredictions), color = "black") +
  geom_point(alpha = 0.3, size = 3) +
  facet_wrap(~total) +
  theme_bw()

fit <- fitted(model.state.intercept, total, type="response")
State_IncomePerCap = total$State_IncomePerCap
Race_Of_Deceased = total$raceethnicity
Race_Of_Deceased = ifelse(Race_Of_Deceased == 0, "Not Black", "Black")
df = data.frame(State_IncomePerCap, Race_Of_Deceased, fit) 

ggplot(df, aes(fit, State_IncomePerCap)) +
  geom_point(aes(color = Race_Of_Deceased))


fit <- fitted(model.state.intercept, total, type="response")
Census_Level_Income = total$IncomePerCap
Race_Of_Deceased = total$raceethnicity
Race_Of_Deceased = ifelse(Race_Of_Deceased == 0, "Not Black", "Black")

df = data.frame(Census_Level_Income, Race_Of_Deceased, fit) 

ggplot(df, aes(fit, Census_Level_Income)) +
  geom_point(aes(color = factor(Race_Of_Deceased)))



fit <- fitted(model.state.intercept, total, type="response")
State_IncomePerCap = total$State_IncomePerCap
Race_Of_Deceased = total$raceethnicity
Race_Of_Deceased = ifelse(Race_Of_Deceased == 0, "Not Black", "Black")
df = data.frame(State_IncomePerCap, Race_Of_Deceased, fit) 

ggplot(df, aes(fit, State_IncomePerCap)) +
  geom_point(aes(color = Race_Of_Deceased))


fit <- fitted(model.state.intercept, total, type="response")
Census_Level_Income = total$IncomePerCap
Race_Of_Deceased = total$raceethnicity
Race_Of_Deceased = ifelse(Race_Of_Deceased == 0, "Not Black", "Black")

df = data.frame(Census_Level_Income, Race_Of_Deceased, fit) 

ggplot(df, aes(fit, Census_Level_Income)) +
  geom_point(aes(color = factor(Race_Of_Deceased)))

police = read.csv("police_killings_cleaned.csv")
ggplot(police, aes(x = nat_bucket,fill = raceethnicity)) +geom_bar(position = "dodge")

ggplot(police, aes(x = nat_bucket,fill = raceethnicity)) +geom_bar()

ggplot(police, aes(x = county_bucket,fill = raceethnicity)) +geom_bar(position = "dodge")

ggplot(police, aes(x = county_bucket,fill = raceethnicity)) +geom_bar()

acs = read.csv("acs2015_census_tract_data.csv")
acs = na.omit(acs)
counties = acs

Counties_Income = acs %>%
  group_by(County) %>% 
  summarise(TotalCounty = sum(TotalPop),
            County_IncomePerCap = sum(TotalPop*IncomePerCap)/TotalCounty,
            twenty = quantile(County_IncomePerCap,0.2,na.rm=TRUE),
            forty=quantile(County_IncomePerCap,0.4,na.rm=TRUE),
            sixty=quantile(County_IncomePerCap,0.6,na.rm=TRUE),
            eighty=quantile(County_IncomePerCap,0.8,na.rm=TRUE))

counties = merge(Counties_Income,counties, by= "County") 
counties$county_bucket[counties$IncomePerCap < counties$twenty] = 1
counties$county_bucket[counties$IncomePerCap >= counties$twenty & counties$IncomePerCap < counties$forty] = 2
counties$county_bucket[counties$IncomePerCap>=counties$forty & counties$IncomePerCap < counties$sixty] = 3
counties$county_bucket[counties$IncomePerCap>=counties$sixty & counties$IncomePerCap < counties$eighty] = 4
counties$county_bucket[counties$IncomePerCap>=counties$eighty]=5


ggplot(counties, aes(x = county_bucket)) +geom_bar()

