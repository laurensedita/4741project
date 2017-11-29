setwd('/home/kristine/Documents/17-18/4741project')
library(boot)
library(glmnet)
library(bestglm)
#project <- read.csv('final_var/var_string_hgr_null.csv',header=TRUE)
project <- read.csv('final_var/male_only.csv',header=TRUE,sep=';')

#names(project) <- c('childid','income','ssat','sus','hgrthink','hgrlike','ncv','hgp','hgr','gradhs','school','momrace','colgrad')

attach(project)
#varnames <- c('childid','childhood income','school satisfication','# of suspensions','highest grade R thinks will complete','highest grade R would like to complete','neighborhood crime and violence','race of mom','highest grade completed by parent','highest grade completed by r','did r graduate highschool','type of school')

#Correlations: health and region, urban/rural and race, and sex and race

#print(colnames(project))

#project$sex = as.factor(project$SAMPLE_SEX)
project$race = as.factor(project$SAMPLE_RACE)
project$urb = as.factor(project$URBAN_RURAL)
project$region = as.factor(project$REGION)
project$marriage = as.factor(project$MARSTAT_COL)
project$unemp = as.factor(project$UNEMP)
project$health = as.factor(project$HEALTHLIMIT)
project$highestgrade = as.factor(project$HIGHESTGRADE)
project$industry = as.factor(project$INDUSTRY)
#print(colnames(project))

#attach(project)
#(str(project))

#test.data = project[,c('INCOME','AFQT','ASVAB','AGE_ENT','health','marriage','region','urb','sex','race','unemp')]

#test.data = project[,c('AFQT','ASVAB','health','AGE_ENT','marriage','industry','highestgrade','unemp')]

test.data = project[,c('INCOME','AFQT','ASVAB','AGE_ENT','YR_OUT','health','marriage','region','industry','highestgrade','race','urb','unemp')]

#str(test.data)
log.m = bestglm(Xy = test.data, IC="AIC", family=binomial,method = 'exhaustive')
log.m$BestModels
summary(log.m$BestModel)

log2.m = bestglm(Xy = test.data, IC="BIC", family=binomial,method = 'exhaustive')
log2.m$BestModels
summary(log2.m$BestModel)

