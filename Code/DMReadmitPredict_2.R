###############################################################################
#
# Project: 	ACO Projects
# Script:	DMReadmitPredict_2.R
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
library(data.table)
library(data.table)
library(AppliedPredicitveModeling)
library(gbm)
library(ada)


load("DMReadmitDataPrepedSample.rda")
#load("DMReadmitDataPrepedAll.rda")


readmitdata <- as.data.frame(subset(dataset, select = c("race","gender","age","admission_type_id",
						"discharge_disposition_id","admission_source_id","time_in_hospital","num_lab_procedures",
						"num_procedures","num_medications","number_outpatient","number_emergency","number_inpatient",
						"diag_1","diag_2","diag_3","number_diagnoses","max_glu_serum","A1Cresult",  
						"metformin","repaglinide","nateglinide","chlorpropamide","glimepiride","acetohexamide",
						"glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose",
						"miglitol","troglitazone","tolazamide","examide","citoglipton","insulin","glyburide_metformin",
						"glipizide_metformin","glimepiride_pioglitazone","metformin_rosiglitazone","metformin_pioglitazone",
						"change","diabetesMed","readmitted","max_glu_serum_measured","readmitAsCoded","readmitAny",
						"readmit30Day","max_glu_serum_m","A1Cresult_m","metformin_m","repaglinide_m", "nateglinide_m",
						"chlorpropamide_m","glimepiride_m","glipizide_m","glyburide_m","tolbutamide_m","pioglitazone_m",
						"rosiglitazone_m","acarbose_m","miglitol_m","troglitazone_m","insulin_m","glyburide_metformin_m",
						"glipizide_metformin_m","glimepiride_pioglitazone_m","metformin_rosiglitazone_m", 
						"metformin_pioglitazone_m")))

readmitdata <- readmitdata[1:500,]

modeldata <- na.omit(subset(readmitdata, select = c("race","gender","age","admission_type_id",
						"discharge_disposition_id","admission_source_id","num_procedures","num_medications",
						"number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","readmit30Day")))

features <- subset(modeldata, select = c("race","gender","age","admission_type_id",
				"discharge_disposition_id","admission_source_id","num_procedures",
				"num_medications","number_inpatient","number_diagnoses","diag_1",
				"diag_2","diag_3"))

target <- modeldata$readmit30Day

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

models <- list(ada = adaFit)
testPred <- predict(models, newdata = testFeatures)
predValues <- extractPrediction(models, testX = testFeatures, testY = testTarget)
testValues <- subset(predValues, dataType == "Test")
head(testValues)
table(testValues$model)
nrow(testFeatures)

adaConfusionMatrix <- confusionMatrix(testValues$pred, testValues$obs)
adaConfusionMatrix

probValues <- extractProb(models,testX = testFeatures, testY = testTarget)
testProbs <- subset(probValues,	dataType == "Test")
str(testProbs)

adaROC <- roc(testProbs$obs, testProbs$FALSE., plot = TRUE)



