#Feature Hashing

setwd("E:/R Directory/Reusables")

require(RCurl)
binData <- getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",
                        ssl.verifypeer=FALSE)

conObj <- file("dataset_diabetes.zip", open = "wb")
writeBin(binData, conObj)
# don't forget to close it
close(conObj)

# open diabetes file
files <- unzip("dataset_diabetes.zip")
diabetes <- read.csv(files[1], stringsAsFactors = FALSE)

# drop useless variables
diabetes <- subset(diabetes,select=-c(encounter_id, patient_nbr))

# transform all "?" to 0s
diabetes[diabetes == "?"] <- NA

# remove zero variance - ty James http://stackoverflow.com/questions/8805298/quickly-remove-zero-variance-variables-from-a-data-frame
diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]

# prep outcome variable to those readmitted under 30 days
diabetes$readmitted <- ifelse(diabetes$readmitted == "<30",1,0)

outcomeName <- 'readmitted'

#Hashing Code
diabetes_hash <- diabetes
predictorNames <- setdiff(names(diabetes_hash),outcomeName)

# change all NAs to 0
diabetes_hash[is.na(diabetes_hash)] <- 0

set.seed(1234)
split <- sample(nrow(diabetes_hash), floor(0.5*nrow(diabetes_hash)))
objTrain <-diabetes_hash[split,]
objTest <- diabetes_hash[-split,]

library(FeatureHashing)
objTrain_hashed = hashed.model.matrix(~., data=objTrain[,predictorNames], hash.size=2^12, transpose=FALSE)
objTrain_hashed = as(objTrain_hashed, "dgCMatrix")
objTest_hashed = hashed.model.matrix(~., data=objTest[,predictorNames], hash.size=2^12, transpose=FALSE)
objTest_hashed = as(objTest_hashed, "dgCMatrix")

library(glmnet)
glmnetModel <- cv.glmnet(objTrain_hashed, objTrain[,outcomeName], 
                         family = "binomial", type.measure = "auc")
#Scoring
glmnetPredict <- predict(glmnetModel, objTest_hashed, s="lambda.min")
auc(objTest[,outcomeName], glmnetPredict)

