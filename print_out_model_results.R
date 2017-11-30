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


model8 = glm(unemp~INCOME+health+marriage+industry+highestgrade+race,data=project,family=binomial)

summary(model8)


