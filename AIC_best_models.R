setwd('/home/kristine/Documents/17-18/4741project')
library(boot)
library(glmnet)
library(bestglm)
project <- read.csv('final_var/male_only.csv',header=TRUE,sep=';')

attach(project)

project$race = as.factor(project$SAMPLE_RACE)
project$urb = as.factor(project$URBAN_RURAL)
project$region = as.factor(project$REGION)
project$marriage = as.factor(project$MARSTAT_COL)
project$unemp = as.factor(project$UNEMP)
project$health = as.factor(project$HEALTHLIMIT)
project$highestgrade = as.factor(project$HIGHESTGRADE)
project$industry = as.factor(project$INDUSTRY)


#Variable_names = 'INCOME','AFQT','ASVAB','AGE_ENT','YR_OUT','health','marriage','region','industry','highestgrade','race','urb','unemp')]

income.data = project[,c('INCOME','health','marriage','region','industry','highestgrade','race','urb','unemp')]
afqt.data = project[,c('AFQT','health','marriage','region','industry','highestgrade','race','urb','unemp')]
asvab.data = project[,c('ASVAB','health','marriage','region','industry','highestgrade','race','urb','unemp')]
yrout.data = project[,c('YR_OUT','health','marriage','region','industry','highestgrade','race','urb','unemp')]
ageent.data = project[,c('AGE_ENT','health','marriage','region','industry','highestgrade','race','urb','unemp')]

log.income = bestglm(Xy=income.data, IC="AIC", family=binomial,method = 'exhaustive')
log.income$BestModels
log.afqt = bestglm(Xy=afqt.data, IC="AIC", family=binomial,method = 'exhaustive')
log.afqt$BestModels
log.asvab = bestglm(Xy=asvab.data, IC="AIC", family=binomial,method = 'exhaustive')
log.asvab$BestModels
log.yrout = bestglm(Xy=yrout.data, IC="AIC", family=binomial,method = 'exhaustive')
log.yrout$BestModels
log.ageent = bestglm(Xy=ageent.data, IC="AIC", family=binomial,method = 'exhaustive')
log.ageent$BestModels


