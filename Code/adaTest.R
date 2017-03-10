###############################################################################
#
# Project: 	ACO Projects
# Script:	adaTest.R
# Version:	
# Created:	
# Updated:	Dec 4, 2014
# Author: 	RY5T
# Copyright University of Virginia, 2014
###############################################################################
library(caret)
library(data.table)
library(icd9)
library(data.table)
library(AppliedPredicitveModeling)
library(gbm)
library(ada)

data(iris)
iris[iris$Species!="setosa",]->iris
n<-dim(iris)[1]
trind<-sample(1:n,floor(.6*n),FALSE)
teind<-setdiff(1:n,trind)
iris[,5]<- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])
gdis<-ada(Species~.,data=iris[trind,],iter=20,nu=1,type="discrete")
#gdis=addtest(gdis,iris[teind,-5],iris[teind,5])
x11()
plot(gdis,FALSE,FALSE)
x11()
varplot(gdis)
#pairs(gdis,iris[trind,-5],maxvar=2)

