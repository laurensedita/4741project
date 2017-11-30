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

sapply(project, class)
#MODELS
#  YR_OUT health marriage region industry highestgrade race   urb Criterion
#1   TRUE   TRUE     TRUE   TRUE     TRUE         TRUE TRUE FALSE  3160.653
#2   TRUE   TRUE     TRUE  FALSE     TRUE         TRUE TRUE FALSE  3161.213
#3   TRUE   TRUE     TRUE   TRUE     TRUE         TRUE TRUE  TRUE  3162.171
#4   TRUE   TRUE     TRUE  FALSE     TRUE         TRUE TRUE  TRUE  3163.085
#5   TRUE   TRUE     TRUE   TRUE    FALSE         TRUE TRUE FALSE  3164.121
#  INCOME health marriage region industry highestgrade race   urb Criterion
#1   TRUE   TRUE     TRUE   TRUE     TRUE         TRUE TRUE FALSE  3134.102
#2   TRUE   TRUE     TRUE   TRUE    FALSE         TRUE TRUE FALSE  3134.133
#3   TRUE   TRUE     TRUE  FALSE     TRUE         TRUE TRUE FALSE  3135.539
#4   TRUE   TRUE     TRUE  FALSE    FALSE         TRUE TRUE FALSE  3135.587
#5   TRUE   TRUE     TRUE   TRUE     TRUE         TRUE TRUE  TRUE  3136.076

model1 = glm(unemp~YR_OUT+health+marriage+region+industry+highestgrade+race,data=project,family=binomial)
model2 = glm(unemp~YR_OUT+health+marriage+industry+highestgrade+race,data=project,family=binomial)
model3 = glm(unemp~YR_OUT+health+marriage+region+industry+highestgrade+race+urb,data=project,family=binomial)
model4 = glm(unemp~YR_OUT+health+marriage+industry+highestgrade+race+urb,data=project,family=binomial)
model5 = glm(unemp~YR_OUT+health+marriage+region+highestgrade+race,data=project,family=binomial)

model6 = glm(unemp~INCOME+health+marriage+region+industry+highestgrade+race,data=project,family=binomial)
model7 = glm(unemp~INCOME+health+marriage+region+highestgrade+race,data=project,family=binomial)
model8 = glm(unemp~INCOME+health+marriage+industry+highestgrade+race,data=project,family=binomial)
model9 = glm(unemp~INCOME+health+marriage+highestgrade+race,data=project,family=binomial)
model10 = glm(unemp~INCOME+health+marriage+region+industry+highestgrade+race+urb,data=project,family=binomial)



alpha = c(.36,.38,.40,.42,.44,.46,.48,.5,.52,.54,.56,.58,.6)
modelcverror1 <- c(1:length(alpha))
modelcverror2 <- c(1:length(alpha))
modelcverror3 <- c(1:length(alpha))
modelcverror4 <- c(1:length(alpha))
modelcverror5 <- c(1:length(alpha))
modelcverror6 <- c(1:length(alpha))
modelcverror7 <- c(1:length(alpha))
modelcverror8 <- c(1:length(alpha))
modelcverror9 <- c(1:length(alpha))
modelcverror10 <- c(1:length(alpha))


mycost <- function(r, pi){
	 weight1 = 1 #cost for getting 1 wrong
	 weight0 = 1 #cost for getting 0 wrong
	 c1 = (r==1)&(pi<j) #logical vector - true if actual 1 but predict 0
	 c0 = (r==0)&(pi>j) #logical vector - true if actual 0 but predict 1
	 return(mean(weight1*c1+weight0*c0))
	 }

count=1
for (j in alpha){
	print(j)
	modelcverror1[count] = cv.glm(project, model1, cost=mycost, K=251)$delta[1]
	modelcverror2[count] = cv.glm(project, model2, cost=mycost, K=251)$delta[1]
	modelcverror3[count] = cv.glm(project, model3, cost=mycost, K=251)$delta[1]
	modelcverror4[count] = cv.glm(project, model4, cost=mycost, K=251)$delta[1]
	modelcverror5[count] = cv.glm(project, model5, cost=mycost, K=251)$delta[1]
	modelcverror6[count] = cv.glm(project, model6, cost=mycost, K=251)$delta[1]
	modelcverror7[count] = cv.glm(project, model7, cost=mycost, K=251)$delta[1]
	modelcverror8[count] = cv.glm(project, model8, cost=mycost, K=251)$delta[1]
	modelcverror9[count] = cv.glm(project, model9, cost=mycost, K=251)$delta[1]
	modelcverror10[count] = cv.glm(project, model10, cost=mycost, K=251)$delta[1]
	count = count+1
}
print(modelcverror1)
print(modelcverror2)
print(modelcverror3)
print(modelcverror4)
print(modelcverror5)
print("         ")
print(modelcverror5)
print(modelcverror6)
print(modelcverror7)
print(modelcverror8)
print(modelcverror9)
print(modelcverror10)

b1 = which.min(modelcverror1)
b2 = which.min(modelcverror2)
b3 = which.min(modelcverror3)
b4 = which.min(modelcverror4)
b5 = which.min(modelcverror5)
b6 = which.min(modelcverror6)
b7 = which.min(modelcverror7)
b8 = which.min(modelcverror8)
b9 = which.min(modelcverror9)
b10 = which.min(modelcverror10)
print(1-modelcverror1[b1])
print(alpha[b1])
print(1-modelcverror2[b2])
print(alpha[b2])
print(1-modelcverror3[b3])
print(alpha[b3])
print(1-modelcverror4[b4])
print(alpha[b4])
print(1-modelcverror5[b5])
print(alpha[b5])
print(1-modelcverror6[b6])
print(alpha[b6])
print(1-modelcverror7[b7])
print(alpha[b7])
print(1-modelcverror8[b8])
print(alpha[b8])
print(1-modelcverror9[b9])
print(alpha[b9])
print(1-modelcverror10[b10])
print(alpha[b10])
warnings()


