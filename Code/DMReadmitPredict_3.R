###############################################################################
#
# Project: 	ACO Projects
# Script:	DMReadmitPredict_3.R
# Version:	
# Created:	Dec 02, 2014
# Updated:	Dec 16, 2014
# Author: 	RY5T
# Notes:	This version uses only the features in the Cerner data set that
# 			are expected to be available in the ACO data set.
#
# Copyright University of Virginia, 2014
###############################################################################

library(caret)
library(pROC)
library(plyr)
library(data.table)
library(gbm)
library(ada)

dataSet <- fread("DMReadmitDataInput_2.csv", header = TRUE)

cols <- c('race','gender','admission_type_id','discharge_disposition_id','admission_source_id','payer_code','medical_specialty',
		  'diag_1','diag_2','diag_3','max_glu_serum','A1Cresult','change','diabetesMed','readmitted30Day','readmittedAny')  

dataSet[,(cols):=lapply(.SD, as.factor),.SDcols=cols]


readmitData <- dataSet[1:100,]


modeldata <- as.data.frame(na.omit(subset(readmitData, select = c("race","gender","age","admission_type_id",
						"discharge_disposition_id","admission_source_id","num_procedures","num_medications",
						"number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","readmitted30Day"))))

features <- as.data.frame(subset(modeldata, select = c("race","gender","age","admission_type_id",
				"discharge_disposition_id","admission_source_id","num_procedures",
				"num_medications","number_inpatient","number_diagnoses","diag_1",
				"diag_2","diag_3")))

target <- modeldata$readmitted30Day

set.seed(1)
##-------------------------------------
## Setup traing and test partitions
##-------------------------------------
inTrain <- createDataPartition(target, p = 3/4, list = FALSE)

trainFeatures <- features[inTrain,]
testFeatures  <- features[-inTrain,]
trainTarget <- target[inTrain]
testTarget  <- target[-inTrain]
prop.table(table(target))
prop.table(table(trainTarget))
ncol(trainFeatures)

##----------------------------
##	Test feature distribution
##----------------------------
nearZeroVar(features, saveMetric = TRUE)

##------------------------------
## Model Creation & Evaluation
##------------------------------

fitControl <- trainControl(method = "boot632", number = 10)
adaGrid <- expand.grid(iter = c(10,50,150,200), maxdepth = c(1,2,3,5,10), nu = 1)
adaFit <- train(trainFeatures, trainTarget,
		method = "ada",
		trControl = fitControl,
		tuneGrid = adaGrid,
		verbose = TRUE)

adaFit
# adaFit$finalModel
x11()
plot(adaFit)
x11()
plot(adaFit$finalModel,TRUE,TRUE)
x11()
resampleHist(adaFit)
x11()
dotPlot(varImp(adaFit))

testPred <- predict(adaFit, newdata = testFeatures)
testValues <- subset(predValues, dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testFeatures)

probValues <- extractProb(adaFit,testX = testFeatures, testY = testTarget)
testProbs <- subset(probValues,	dataType == "Test")
str(testProbs)



adaConfusionMatrix <- confusionMatrix(testValues$pred, testValues$obs)
adaConfusionMatrix
adaROC <- roc(testProbs$obs, testProbs$No, plot = TRUE)



