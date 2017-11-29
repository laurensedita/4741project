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
#print(colnames(project))

#attach(project)
#(str(project))

sapply(project, class)


#BIC ONES
#model1 = glm(unemp~AFQT+ASVAB+AGE_ENT+marriage+health,data=project,family=binomial)
#model2 = glm(unemp~AFQT+ASVAB+AGE_ENT+marriage,data=project,family=binomial)
#model3 = glm(unemp~AFQT+AGE_ENT+marriage+health,data=project,family=binomial)
#model4 = glm(unemp~AFQT+AGE_ENT+marriage,data=project,family=binomial)
#model5 = glm(unemp~AFQT+ASVAB+AGE_ENT+marriage+health+highestgrade,data=project,family=binomial)

#AIC ONES
#  INCOME AFQT ASVAB AGE_ENT YR_OUT health marriage region industry highestgrade   race   urb 	Criterion
#1   TRUE TRUE  TRUE    TRUE   TRUE   TRUE     TRUE   TRUE     TRUE        FALSE   FALSE  FALSE  3023.597
#2   TRUE TRUE  TRUE    TRUE   TRUE   TRUE     TRUE   TRUE     TRUE        FALSE   TRUE   FALSE  3024.329
#3   TRUE TRUE  TRUE    TRUE   TRUE  FALSE     TRUE   TRUE     TRUE        FALSE   FALSE  FALSE  3024.923
#4   TRUE TRUE  TRUE    TRUE   TRUE   TRUE     TRUE   TRUE     TRUE        FALSE   FALSE  TRUE   3025.234
#5   TRUE TRUE  TRUE    TRUE   TRUE  FALSE     TRUE   TRUE     TRUE        FALSE   TRUE   FALSE  3025.937
model1 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health,data=project,family=binomial)
model2 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health+race,data=project,family=binomial)
model3 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry,data=project,family=binomial)
model4 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+health+urb,data=project,family=binomial)
model5 = glm(unemp~INCOME+YR_OUT+AFQT+ASVAB+AGE_ENT+marriage+region+industry+race,data=project,family=binomial)

#alpha = c(0.35,0.36,0.37,0.38,0.39,0.40,.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)
#alpha = c(0.3,0.35,0.36,0.37,0.38,0.39,0.40,0.45,.46,.47,.48,.49,.50,.51)
#alpha = c(.35,.4,.45,.5,.55,.6)
alpha = c(.36,.38,.40,.42,.44,.46,.48,.5,.52,.54,.56,.58,.6)
modelcverror1 <- c(1:length(alpha))
modelcverror2 <- c(1:length(alpha))
modelcverror3 <- c(1:length(alpha))
modelcverror4 <- c(1:length(alpha))
modelcverror5 <- c(1:length(alpha))

mycost <- function(r, pi){
	 weight1 = 1 #cost for getting 1 wrong
	 weight0 = 1 #cost for getting 0 wrong
	 c1 = (r==1)&(pi<j) #logical vector - true if actual 1 but predict 0
	 c0 = (r==0)&(pi>j) #logical vector - true if actual 0 but predict 1
	 return(mean(weight1*c1+weight0*c0))
	 }

count=1
#for (k in 1:20){
for (j in alpha){
	print(j)
	modelcverror1[count] = cv.glm(project, model1, cost=mycost, K=25)$delta[1]
	modelcverror2[count] = cv.glm(project, model2, cost=mycost, K=25)$delta[1]
	modelcverror3[count] = cv.glm(project, model3, cost=mycost, K=25)$delta[1]
	modelcverror4[count] = cv.glm(project, model4, cost=mycost, K=25)$delta[1]
	modelcverror5[count] = cv.glm(project, model5, cost=mycost, K=25)$delta[1]
	#modelcverror[count] = cv.glm(project, model1, cost=mycost, K=num_people)$delta[1]  or K=251
	count = count+1
}
print(modelcverror1)
print(modelcverror2)
print(modelcverror3)
print(modelcverror4)
print(modelcverror5)
b1 = which.min(modelcverror1)
b2 = which.min(modelcverror2)
b3 = which.min(modelcverror3)
b4 = which.min(modelcverror4)
b5 = which.min(modelcverror5)
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
warnings()

#}
