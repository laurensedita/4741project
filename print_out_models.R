setwd('/home/kristine/Documents/17-18/4741project')
library(boot)
library(glmnet)
library(bestglm)
library(ROCR)

project <- read.csv('final_var/male_only.csv',header=TRUE,sep=';')

attach(project)

num_people = nrow(project)
train = sample(1:num_people, (num_people-1)/2)
project$race = as.factor(project$SAMPLE_RACE)
project$region = as.factor(project$REGION)
project$marriage = as.factor(project$MARSTAT_COL)
project$unemp = as.factor(project$UNEMP)
project$health = as.factor(project$HEALTHLIMIT)
project$highestgrade = as.factor(project$HIGHESTGRADE)
project$industry = as.factor(project$INDUSTRY)
project$urb = as.factor(project$URBAN_RURAL)
attach(project)

sapply(project, class)


model1 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health,data=project,family=binomial)
model2 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health+race,data=project,family=binomial)
model3 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry,data=project,family=binomial)
model4 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health+urb,data=project,family=binomial)
model5 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+race,data=project,family=binomial)


summary(model1)

summary(model2)
