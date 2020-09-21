setwd("~/Desktop/Econ 2509 Data")
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
rawfludata <- read.csv("~/Downloads/h7n9.csv")
#Density Plot
ggplot(rawfludata, aes(age)) + geom_density(aes(fill=outcome), alpha=1/3)
#Bar Chart Outcome by Gender (0 being Female, 1 being Male) and province
ggplot(rawfludata, aes(male)) + geom_bar(aes(fill=outcome), position="dodge") + facet_wrap(~province)
# First just by age
ggplot(rawfludata, aes(province, age)) + geom_boxplot()
# Then by age and outcome
ggplot(rawfludata, aes(province, age)) + geom_boxplot(aes(fill=outcome))

flu <- read_csv("h7n9_analysisready.csv")
library(psych)
describe(flu$age)
describe(flu$days_to_hospital)
describe(flu$days_to_outcome)
attach(flu)
# Hospital, male, provinces and early outcome are dummy variables

flutrans <- read_csv("h7n9_analysisready.csv")
newcol<-replace_na(flutrans$outcome, "none")
View(newcol)
flunew <- cbind(flutrans,newcol)
View(flunew)
model.matrix(~0+newcol, data=flunew)
cbind(flunew, model.matrix(~0+newcol, data=flunew))
flu2 <- cbind(flunew, model.matrix(~0+newcol, data=flunew)) %>%
  as_tibble() %>%
  select(-newcol)
View(flu2)

# days in hospital = days to outcome - days to hospital
detach(flu)
attach(flu2)
days_in_hospital <- days_to_outcome - days_to_hospital
flu2 <- cbind(flu2, days_in_hospital)
linreg <- lm(age~days_in_hospital)
summary(linreg)
ggplot(flu2,aes(x = age, y = days_in_hospital)) + geom_point(color='blue') 
+ geom_line(color='red',data = flu2, aes(x=age, y=days_in_hospital))
probit1 <- glm(newcolDeath~age)
summary(probit1)
probit2 <- glm(newcolDeath~age + male + early_outcome + hospital)
summary (probit2)

#Interpreting Probit Results
library(erer)
fm3a = maBina(probit1, x.mean=FALSE,rev.dum = TRUE,digits = 3)


