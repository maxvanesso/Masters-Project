# Setting the working directory
setwd("~/Escritorio/Data Science/Masters_Project")

# Loading the data and changing possible blanks for NA
data <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

# Loading packages
library(caret)
library(e1071)
library(splines)
library(plyr)
library(parallel)
library(gbm)
library(corrplot)
library(ranger)
library(caTools)
library(mlr)

# Setting a seed to make the models that use randomization comparable between each other
set.seed(826)

# Check for how many NA are in the dataset
sum(is.na(data))

# Fix a misspelled colname
index <- which(colnames(data) == 'MalaltiaRespitatoria')
colnames(data)[index] <- 'MalaltiaRespiratoria'

# Replace some values in a column
data$PCC <- replace(data$PCC, data$PCC == "S", "Y")
data$MACA <- replace(data$MACA, data$MACA == "S", "Y")
data$ServeiOnco <- replace(data$ServeiOnco, data$ServeiOnco == "S", "Y")
data[,13:143] <- replace(data[,13:143], data[,13:143] == 2, 0)

# Take out the two columns that we don't need: DataIngres and DataAlta
data <- data[,c(1:158, 160:164, 166:183)]

# Change the columns to factors and bind them with the non factors (discrete and continuous)
data <- cbind(data[,-c(2:143,145:157,159:168,170:172,179:181)],
              data.frame(apply(data[,c(2:143,145:157,159:168,170:172,179:181)], 2, factor)))

# Build a matrix for each response (Length of Stay (LoS), Readmission Type 1 (R1), Readmission Type 2 (R2), Readmission Type 3 (R3))
LoS <- data[,-c(179:181)] # The response variable is the 4th column
LoS <- LoS[,-10] # Take out the DiesReadmissio variable
LoS <- LoS[-17383,] # Eliminate the row 17383 because it has a LoS of -1
LoS$DuradaEstada <- cut(LoS[,4], breaks = c(-1, 2, 7, 133), labels = c("Less than 2 days", "Between 3 to 7 days", "More than 1 week")) # Transform the LoS variable into factor intervals labeled
R1 <- data[,-c(4,180,181)] # The response variable is the 178th column
R1 <- R1[,-9] # Take out the DiesReadmissio variable
R2 <- data[,-c(4,179,181)] # The response variable is the 178th column
R2 <- R2[,-9] # Take out the DiesReadmissio variable
R3 <- data[,-c(4,179,180)] # The response variable is the 178th column
R3 <- R3[,-9] # Take out the DiesReadmissio variable

# Build a dataset with only the non-numeric variables
datacategoric <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

datacategoric <- datacategoric[,-c(175:183, 173, 171:168, 164:162, 158:13, 1, 2)]

####################################
######### CORRELATION ##############
####################################

# Checking correlation and fisher information for variable selection

# Loading the data and changing possible blanks for NA
datanumeric <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

index <- which(colnames(datanumeric) == 'MalaltiaRespitatoria')
colnames(datanumeric)[index] <- 'MalaltiaRespiratoria'


datanumeric$ServeiOnco <- replace(datanumeric$ServeiOnco, datanumeric$ServeiOnco == "S", 1)
datanumeric$ServeiOnco <- replace(datanumeric$ServeiOnco, datanumeric$ServeiOnco == "N", 0)
datanumeric$Sexe <- replace(datanumeric$Sexe, datanumeric$Sexe == "H", 1)
datanumeric$Sexe <- replace(datanumeric$Sexe, datanumeric$Sexe == "D", 0)
datanumeric$CondClinEspec <- replace(datanumeric$CondClinEspec, datanumeric$CondClinEspec == "Y", 1)
datanumeric$CondClinEspec <- replace(datanumeric$CondClinEspec, datanumeric$CondClinEspec == "N", 0)
datanumeric$MalaltiaMental <- replace(datanumeric$MalaltiaMental, datanumeric$MalaltiaMental == "Y", 1)
datanumeric$MalaltiaMental <- replace(datanumeric$MalaltiaMental, datanumeric$MalaltiaMental == "N", 0)
datanumeric$AlcoholDrogues <- replace(datanumeric$AlcoholDrogues, datanumeric$AlcoholDrogues == "Y", 1)
datanumeric$AlcoholDrogues <- replace(datanumeric$AlcoholDrogues, datanumeric$AlcoholDrogues == "N", 0)
datanumeric$Neoplasia <- replace(datanumeric$Neoplasia, datanumeric$Neoplasia == "Y", 1)
datanumeric$Neoplasia <- replace(datanumeric$Neoplasia, datanumeric$Neoplasia == "N", 0)
datanumeric$RetardMental <- replace(datanumeric$RetardMental, datanumeric$RetardMental == "Y", 1)
datanumeric$RetardMental <- replace(datanumeric$RetardMental, datanumeric$RetardMental == "N", 0)
datanumeric$MalaltiaRespiratoria <- replace(datanumeric$MalaltiaRespiratoria, datanumeric$MalaltiaRespiratoria == "Y", 1)
datanumeric$MalaltiaRespiratoria <- replace(datanumeric$MalaltiaRespiratoria, datanumeric$MalaltiaRespiratoria == "N", 0)
datanumeric$Diabetis <- replace(datanumeric$Diabetis, datanumeric$Diabetis == "Y", 1)
datanumeric$Diabetis <- replace(datanumeric$Diabetis, datanumeric$Diabetis == "N", 0)
datanumeric$IC <- replace(datanumeric$IC, datanumeric$IC == "Y", 1)
datanumeric$IC <- replace(datanumeric$IC, datanumeric$IC == "N", 0)
datanumeric$PerduaPes <- replace(datanumeric$PerduaPes, datanumeric$PerduaPes == "Y", 1)
datanumeric$PerduaPes <- replace(datanumeric$PerduaPes, datanumeric$PerduaPes == "N", 0)
datanumeric$Depressio <- replace(datanumeric$Depressio, datanumeric$Depressio == "Y", 1)
datanumeric$Depressio <- replace(datanumeric$Depressio, datanumeric$Depressio == "N", 0)
datanumeric$Anemia <- replace(datanumeric$Anemia, datanumeric$Anemia == "Y", 1)
datanumeric$Anemia <- replace(datanumeric$Anemia, datanumeric$Anemia == "N", 0)
datanumeric$PCC <- replace(datanumeric$PCC, datanumeric$PCC == "S", 1)
datanumeric$PCC <- replace(datanumeric$PCC, datanumeric$PCC == "N", 0)
datanumeric$MACA <- replace(datanumeric$MACA, datanumeric$MACA == "S", 1)
datanumeric$MACA <- replace(datanumeric$MACA, datanumeric$MACA == "N", 0)
datanumeric$ReadmissioN1 <- replace(datanumeric$ReadmissioN1, datanumeric$ReadmissioN1 == "Y", 1)
datanumeric$ReadmissioN1 <- replace(datanumeric$ReadmissioN1, datanumeric$ReadmissioN1 == "N", 0)
datanumeric$ReadmissioN2 <- replace(datanumeric$ReadmissioN2, datanumeric$ReadmissioN2 == "Y", 1)
datanumeric$ReadmissioN2 <- replace(datanumeric$ReadmissioN2, datanumeric$ReadmissioN2 == "N", 0)
datanumeric$ReadmissioN3 <- replace(datanumeric$ReadmissioN3, datanumeric$ReadmissioN3 == "Y", 1)
datanumeric$ReadmissioN3 <- replace(datanumeric$ReadmissioN3, datanumeric$ReadmissioN3 == "N", 0)

datanumeric <- datanumeric[,c(175:183, 173, 171:168, 164:162, 158:13, 1, 2)] # subsetting all the variables that can be numeric/binary

datanumeric <- sapply(datanumeric, as.numeric)

datanumeric <- as.data.frame(datanumeric)

datanumeric <- datanumeric[,-6] # Take out DiesReadmissio

# We select the 150 with var > 0 if we take out the nzv as well we will end up with the 49 variables
cornzvresult <- nzv(datanumeric, saveMetrics = TRUE) # store the results for nzv

corzerovar150 <- row.names(cornzvresult[cornzvresult$zeroVar == TRUE,]) # subset only the variables with zero variance

datanumericnzv150 <- datanumeric[,!colnames(datanumeric) %in% corzerovar150] # keep only the columns whose names are different from those in the zero variance object

correlation150 <- cor(datanumericnzv150, use = "pairwise.complete.obs") # if there are NA the correlation is computed only with the completed pairs of the two columns

corrplot(correlation150, method="square", tl.cex = 0.3, order = "hclust") # How to visualize it better?

# getting the max value for each row/column

correlation150.1 <- data.frame(correlation150 - diag(1, 150, 150))

maxvalues150 <- apply(correlation150.1, 2, max)

# try with apply and sapply
maxs<-apply(correlation150.1,2,function(x)return(array(which.max(x))))
maxs<-data.frame(col=names(maxs),row=maxs)
correlation150.1$maxs<-apply(maxs,1,FUN=function(x)return(paste(x["col"],
                                                  rownames(correlation150.1[as.numeric(x["row"]),]),
                                                  correlation150.1[as.numeric(x["row"]),x["col"]],
                                                  sep="/")))

# If you execute first the code above it will show warnings because it coerces NA's
maxscorr <- t(sapply(seq(nrow(correlation150.1)), function(i) {
  j <- which.max(correlation150.1[i,])
  c(paste(rownames(correlation150.1)[i], colnames(correlation150.1)[j], sep='/'), correlation150.1[i,j])
}))


minscorr <- t(sapply(seq(nrow(correlation150.1)), function(i) {
  j <- which.min(correlation150.1[i,])
  c(paste(rownames(correlation150.1)[i], colnames(correlation150.1)[j], sep='/'), correlation150.1[i,j])
}))


####################################
######### FISHER SCORE #############
####################################

# We can only apply this to numerical variables so we are going to create a matrix with the numeric variables and each response
# We first remove the NA to get a Fisher score for all the variables

datanumeric1 <- na.omit(datanumeric)

# We also remove the zero variance variables because otherwise they get a NaN as a Fisher score

nzvresultdn <- nzv(datanumeric1, saveMetrics = TRUE) # store the results for nzv

zerovardn <- row.names(nzvresultdn[nzvresultdn$zeroVar == TRUE,]) # subset only the variables with zero variance

datanumericZV <- datanumeric1[,!colnames(datanumeric1) %in% zerovardn] # keep only the columns whose names are different from those in the zero variance object


# Numeric data and LoS as response
numericLoS <- datanumericZV[,-c(6:8)]

# Building the matrix with top X vars according to Fisher Score, we have to add the response
LoSvars19 <- fisher.selection(numericLoS[,-7], numericLoS[,7], 19, 2.5)
LoSvars19 <- cbind(LoSvars19, numericLoS$DuradaEstada)
colnames(LoSvars19)[20] <- "DuradaEstada"

LoSvars40 <- fisher.selection(numericLoS[,-7], numericLoS[,7], 40, 2.5)
LoSvars40 <- cbind(LoSvars40, numericLoS$DuradaEstada)
colnames(LoSvars40)[41] <- "DuradaEstada"

features = numericLoS[,-7]
labels = numericLoS[,7]
n= 100
threshold = 2.5

plot(top.ranks, type="p", main = "LoS variables Fisher Score", abline(v=c(19, 40), col=3, lty=3))
axis(side=1, at=c(0:147))

# We try a model with 19 variables and a model with 40 variables

# Numeric data and R1 as response
numericR1 <- datanumericZV[,-c(7:8, 10)]

# Building the matrix with top X vars according to Fisher Score, we have to add the response
features = numericR1[,-6]
labels = numericR1[,6]
n= 100
threshold = 0.5

R1vars11 <- fisher.selection(numericR1[,-6], numericR1[,6], 11, 0.5)
R1vars11 <- cbind(R1vars11, numericR1$ReadmissioN1)
colnames(R1vars11)[12] <- "ReadmissioN1"

R1vars23 <- fisher.selection(numericR1[,-6], numericR1[,6], 23, 0.5)
R1vars23 <- cbind(R1vars23, numericR1$ReadmissioN1)
colnames(R1vars23)[24] <- "ReadmissioN1"

plot(top.ranks, type="p", main = "R1 variables Fisher Score", abline(v=c(11, 23), col=3, lty=3))
axis(side=1, at=c(0:147))

# We try a model with 11 variables and a model with 23 variables

# Numeric data and R2 as response
numericR2 <- datanumericZV[,-c(6, 8, 10)]

R2vars13 <- fisher.selection(numericR2[,-6], numericR2[,6], 13, 0.5)
R2vars13 <- cbind(R2vars13, numericR2$ReadmissioN2)
colnames(R2vars13)[14] <- "ReadmissioN2"

R2vars23 <- fisher.selection(numericR2[,-6], numericR2[,6], 23, 0.5)
R2vars23 <- cbind(R2vars23, numericR2$ReadmissioN2)
colnames(R2vars23)[24] <- "ReadmissioN2"

features = numericR2[,-6]
labels = numericR2[,6]
n= 100
threshold = 0.5

plot(top.ranks, type="p", main = "R2 variables Fisher Score", abline(v=c(13, 23), col=3, lty=3))
axis(side=1, at=c(0:147))

# We try a model with 13 variables (and a model with 23 variables???)

# Numeric data and R3 as response
numericR3 <- datanumericZV[,-c(6, 7, 10)]

R3vars12 <- fisher.selection(numericR3[,-6], numericR3[,6], 12, 0.5)
R3vars12 <- cbind(R3vars12, numericR3$ReadmissioN3)
colnames(R3vars12)[13] <- "ReadmissioN3"

R3vars19 <- fisher.selection(numericR3[,-6], numericR3[,6], 19, 0.5)
R3vars19 <- cbind(R3vars19, numericR3$ReadmissioN3)
colnames(R3vars19)[20] <- "ReadmissioN3"

R3vars39 <- fisher.selection(numericR3[,-6], numericR3[,6], 39, 0.5)
R3vars39 <- cbind(R3vars39, numericR3$ReadmissioN3)
colnames(R3vars39)[40] <- "ReadmissioN3"

features = numericR3[,-6]
labels = numericR3[,6]
n= 100
threshold = 0.5

plot(top.ranks, type="p", main = "R3 variables Fisher Score", abline(v=c(12, 19, 39), col=3, lty=3))
axis(side=1, at=c(0:147))

# We try a model with 12 variables (and a model with 19 variables, and a model with 39 variables???)

############################################################
############## MODEL BUILDING AND APPLICATION ##############
############################################################

####################################
######### LENGTH OF STAY ###########
####################################

# Check why there are NA in the response variable

which(is.na(LoS[,4]), arr.ind = T)

# 16471: Missing date of entry
# 17026: Missing date of entry
# 17129: Missing date of entry
# 17217: Missing date of entry
# 17329: Missing date of entry
# 17458: Missing date of entry
# 17531: Missing date of entry
# 17987: Missing date of entry
# 18064: Missing date of entry
# 18074: Missing date of entry
# 18268: Missing date of entry
# 18542: Missing date of entry
# 18676: Missing date of entry
# 18763: Missing date of entry

###################################################
############## COST MATRIX ########################
###################################################

CostMatrix <- matrix(c(0,10,1,0), nrow = 2, ncol = 2, byrow = T)


# Delete the rows with NA in the response

LoS <- LoS[-c(which(is.na(LoS[,4]), arr.ind = T)),]

inTrainingLoS <- createDataPartition(LoS$DuradaEstada, p = .75, list = FALSE)
trainingLoS <- LoS[ inTrainingLoS,]
testingLoS <- na.omit(LoS[-inTrainingLoS,])
test.labels <- testingLoS[,4]
testingLoS <- testingLoS[,-4]

# Trying to solve for factor variables with only 1 factor that cause trouble in the model

nzvresult <- nzv(trainingLoS, saveMetrics = TRUE) # store the results for nzv

zerovar <- row.names(nzvresult[nzvresult$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingLoSnzv <- trainingLoS[,!colnames(trainingLoS) %in% zerovar] # keep only the columns whose names are different from those in the zero variance object

# Fitting the model gbm

fitControlLoS <- trainControl(## 5-fold CV
  method = "cv",
  number = 5, repeats = 1)

gbmFit1LoS <- train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
                    method = "gbm",
                    trControl = fitControlLoS,
                    verbose = TRUE)


# Why does predictionLoS and testingLoS have different length, how can I get a discrete prediction?
predictionsLoSgbm <- predict(gbmFit1LoS, newdata = testingLoS)
MSEgbm <- sum((predictionsLoSgbm-test.labels)^2)/(length(predictionsLoSgbm))
daysoff <- ceiling(predictionsLoSgbm)-test.labels
mean(daysoff)
sd(daysoff)
var(daysoff)
qplot(predictionsLoSgbm,test.labels) + geom_abline(slope = 1, col = 4)

sum(floor(predictionsLoSgbm) == test.labels)/length(test.labels) # If I change floor for ceiling it drops to 0.10
table(test.labels)
table(floor(predictionsLoSgbm))
table(predictionsLoSrf, test.labels)
qplot(predictionsLoSgbm,test.labels, main = "Error types of GBMLoS") + geom_abline(slope = 1, col = 4)

# Trying a rf

rfFit1LoS <- train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
                    method = "ranger",
                    trControl = fitControlLoS)

predictionsLoSrf <- predict(rfFit1LoS, newdata = testingLoS)
MSErf <- sum((predictionsLoSrf-test.labels)^2)/(length(predictionsLoSrf))

sum(floor(predictionsLoSrf) == test.labels)/length(test.labels) # If I change floor for ceiling it drops to 0.15
table(test.labels)
table(floor(predictionsLoSrf))
table(predictionsLoSrf, test.labels)
qplot(predictionsLoSrf,test.labels, main = "Error types of RFLoS") + geom_abline(slope = 1, col = 4)




########################
######### R1 ###########
########################

# Trying a RF with all the variables
R1$ReadmissioN1 <- as.character(R1$ReadmissioN1)
R1$ReadmissioN1[R1$ReadmissioN1 == 'Y'] <- 1
R1$ReadmissioN1[R1$ReadmissioN1 == 'N'] <- 0
R1$ReadmissioN1 <- as.factor(R1$ReadmissioN1)
inTrainingR1 <- createDataPartition(R1$ReadmissioN1, p = .75, list = FALSE)
trainingR1 <- na.omit(R1[ inTrainingR1,])
testingR1 <- na.omit(R1[-inTrainingR1,])
#test.labels1 <- testingR1[,177]
#testingR1 <- testingR1[,-177]

nzvresultR1 <- nzv(trainingR1, saveMetrics = TRUE) # store the results for nzv

zerovarR1 <- row.names(nzvresultR1[nzvresultR1$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR1nzv <- trainingR1[,!colnames(trainingR1) %in% zerovarR1]
testingR1nzv <- testingR1[,!colnames(testingR1) %in% zerovarR1]# keep only the columns whose names are different from those in the zero variance object

fitControlR1 <- trainControl(## 5-fold CV
  method = "cv",
  number = 5, repeats = 1)


rfFit1R1 <- train(trainingR1nzv$ReadmissioN1 ~ ., data = trainingR1nzv,
                    method = "ranger",
                    trControl = fitControlR1,
                    verbose = TRUE)

# With the same seed we reach an accuracy of %
predictionsR1rf <- predict(rfFit1R1, newdata = testingR1)
sum(predictionsR1rf == test.labels1)/length(test.labels1)
table(test.labels1)
table(predictionsR1rf)
table(predictionsR1rf, test.labels1)
qplot(predictionsR1rf,test.labels1, main = "Error types of RF1") + geom_abline(slope = 1, col = 4)

#####################################################
# First try of logistic regression for 11 variables #
#####################################################

# LogisticFit1R1 <- train(trainingR1nzv$ReadmissioN1 ~ ., data = trainingR1nzv,
#                         method = "LogitBoost",
#                         trControl = fitControlR1,
#                         verbose = TRUE)
# 
# # We reach an accuracy of 74.51%
# predictionsR1Logistic <- predict(LogisticFit1R1, newdata = testingR1)
# sum(predictionsR1Logistic == test.labels1)/length(test.labels1)
# table(test.labels1)
# table(predictionsR1Logistic)
# table(predictionsR1Logistic, test.labels1)
# qplot(predictionsR1Logistic,test.labels1, main = "Error types of Logistic1") + geom_abline(slope = 1, col = 4)


########################
######### R2 ###########
########################

# Trying a rf with all the variables for R2 to see what's going on

inTrainingR2 <- createDataPartition(R2$ReadmissioN2, p = .75, list = FALSE)
trainingR2 <- R2[ inTrainingR2,]
testingR2 <- na.omit(R2[-inTrainingR2,])
test.labels2 <- testingR2[,177]
testingR2 <- testingR2[,-177]

nzvresultR2 <- nzv(trainingR2, saveMetrics = TRUE) # store the results for nzv

zerovarR2 <- row.names(nzvresultR2[nzvresultR2$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR2nzv <- trainingR2[,!colnames(trainingR2) %in% zerovarR2] # keep only the columns whose names are different from those in the zero variance object

fitControlR2 <- trainControl(## 5-fold CV
  method = "cv",
  number = 5, repeats = 1)

rfFit1R2 <- train(trainingR2nzv$ReadmissioN2 ~ ., data = trainingR2nzv,
                  method = "ranger",
                  trControl = fitControlR2,
                  verbose = TRUE)

# We reach an accuracy of %
predictionsR2rf <- predict(rfFit1R2, newdata = testingR2)
sum(predictionsR2rf == test.labels2)/length(test.labels2)
table(test.labels2)
table(predictionsR2rf)
table(predictionsR2rf, test.labels2)
qplot(predictionsR2rf,test.labels2, main = "Error types of RF2") + geom_abline(slope = 1, col = 4)



#####################################################
# First try of logistic regression for 13 variables #
#####################################################

# LogisticFit1R2 <- train(trainingR2nzv$ReadmissioN2 ~ ., data = trainingR2nzv,
#                         method = "LogitBoost",
#                         trControl = fitControlR2,
#                         verbose = TRUE)
# 
# # We reach an accuracy of 89.02%
# predictionsR2Logistic <- predict(LogisticFit1R2, newdata = testingR2)
# sum(predictionsR2Logistic == test.labels2)/length(test.labels2)
# table(test.labels2)
# table(predictionsR2Logistic)
# table(predictionsR2Logistic, test.labels2)
# qplot(predictionsR2Logistic,test.labels2, main = "Error types of Logistic2") + geom_abline(slope = 1, col = 4)
# 


########################
######### R3 ###########
########################

# Trying a rf with all the variables for R3 to see what's going on
inTrainingR3 <- createDataPartition(R3$ReadmissioN3, p = .75, list = FALSE)
trainingR3 <- R3[ inTrainingR3,]
testingR3 <- na.omit(R3[-inTrainingR3,])
test.labels3 <- testingR3[,177]
testingR3 <- testingR3[,-177]

nzvresultR3 <- nzv(trainingR3, saveMetrics = TRUE) # store the results for nzv

zerovarR3 <- row.names(nzvresultR3[nzvresultR3$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR3nzv <- trainingR3[,!colnames(trainingR3) %in% zerovarR3] # keep only the columns whose names are different from those in the zero variance object

fitControlR3 <- trainControl(## 5-fold CV
  method = "cv",
  number = 5, repeats = 1)


rfFit1R3 <- train(trainingR3nzv$ReadmissioN3 ~ ., data = trainingR3nzv,
                    method = "ranger",
                    trControl = fitControlR3,
                    verbose = TRUE)

# We reach an accuracy of 0%
predictionsR3rf <- predict(rfFit1R3, newdata = testingR3)
sum(predictionsR3rf == test.labels3)/length(test.labels3)
table(test.labels3)
table(predictionsR3rf)
table(predictionsR3rf, test.labels3)
qplot(predictionsR3rf,test.labels3, main = "Error types of RF3") + geom_abline(slope = 1, col = 4)

#roc.plot(test.labels3, predictionsR3rf) #library verification?


#####################################################
# First try of logistic regression for 12 variables #
#####################################################

LogisticFit1R3 <- train(trainingR3nzv$ReadmissioN3 ~ ., data = trainingR3nzv,
                        method = "LogitBoost",
                        trControl = fitControlR3,
                        verbose = TRUE)

# We reach an accuracy of 100%
predictionsR3Logistic <- predict(LogisticFit1R3, newdata = testingR3)
sum(predictionsR3Logistic == test.labels3)/length(test.labels3)
table(test.labels3)
table(predictionsR3Logistic)
table(predictionsR3Logistic, test.labels3)
qplot(predictionsR3Logistic,test.labels3, main = "Error types of Logistic3") + geom_abline(slope = 1, col = 4)



#####################################################################################################################
# For LoS, R1, R2 and R3 we build a training and test set with all the variables, we took away the NA's from the 
# response variables and we also eliminated the variables with zero variance. After realizing that with datasets of
# 178 variables it was too slow and inefficient to run the models for prediction and also realizing that the 
# importance of the variables was a relevant aspect to consider in order to reduce the size of the model I calculated
# the Fisher score of the numeric variables to do variable selection and reduce the number of features in each model.
# After selecting the variables with higher FS I added the categorical variables to build the final dataset which
# with I am gonna try different models for predicting the response variables.
#####################################################################################################################


# Generate a matrix with the top X numeric variables selected by the FS plus the categorical variables
LoS19 <- cbind(LoS[,colnames(LoS) %in% colnames(LoSvars19)], LoS[,colnames(LoS) %in% colnames(datacategoric)])

inTrainingLoS19 <- createDataPartition(LoS19$DuradaEstada, p = .75, list = FALSE)
trainingLoS19 <- LoS19[ inTrainingLoS19,]
testingLoS19 <- na.omit(LoS19[-inTrainingLoS19,])
test.labels19 <- testingLoS19[,3]
testingLoS19 <- testingLoS19[,-3]

# Trying to solve for factor variables with only 1 factor that cause trouble in the model
nzvresult19 <- nzv(trainingLoS19, saveMetrics = TRUE) # store the results for nzv

zerovar19 <- row.names(nzvresult19[nzvresult19$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingLoS19nzv <- trainingLoS19[,!colnames(trainingLoS19) %in% zerovar19]

LRFit1LoS19 <- train(trainingLoS19nzv$DuradaEstada ~ ., data = trainingLoS19nzv,
                     method = "glm", family = poisson(),
                     trControl = fitControlLoS1)

predictionsLoS19rf <- predict(rfFit1LoS19, newdata = testingLoS19)
MSErf19 <- sum((predictionsLoS19rf-test.labels19)^2)/(length(predictionsLoS19rf))
qplot(predictionsLoS19rf,test.labels19) + geom_abline(slope = 1, col = 4)


########################
######### R1 ###########
########################

# Trying a RF with the best 11 variables from the FS plus the categorical variables
R111 <- cbind(R1[,colnames(R1) %in% colnames(R1vars11)], R1[,colnames(R1) %in% colnames(datacategoric)])

inTrainingR111 <- createDataPartition(R111$ReadmissioN1, p = .75, list = FALSE)
trainingR111 <- R111[ inTrainingR111,]
testingR111 <- na.omit(R111[-inTrainingR111,])
test.labels111 <- testingR111[,12]
testingR111 <- testingR111[,-12]

nzvresultR111 <- nzv(trainingR111, saveMetrics = TRUE) # store the results for nzv

zerovarR111 <- row.names(nzvresultR111[nzvresultR111$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR111nzv <- trainingR111[,!colnames(trainingR111) %in% zerovarR111] # keep only the columns whose names are different from those in the zero variance object

rfFit1R111 <- train(trainingR111nzv$ReadmissioN1 ~ ., data = trainingR111nzv,
                    method = "ranger",
                    trControl = fitControlR1,
                    verbose = TRUE)

# With the same seed we reach an accuracy of 62.07%
predictionsR111rf <- predict(rfFit1R111, newdata = testingR111)
sum(predictionsR111rf == test.labels111)/length(test.labels111)
table(test.labels111)
table(predictionsR111rf)
table(predictionsR111rf, test.labels111)
qplot(predictionsR111rf,test.labels111, main = "Error types of RF111") + geom_abline(slope = 1, col = 4)


LogisticFit1R111 <- train(trainingR111nzv$ReadmissioN1 ~ ., data = trainingR111nzv,
                          method = "LogitBoost",
                          trControl = fitControlR1,
                          verbose = TRUE)

# We reach an accuracy of 72.42%
predictionsR111Logistic <- predict(LogisticFit1R111, newdata = testingR111)
sum(predictionsR111Logistic == test.labels111)/length(test.labels111)
table(test.labels111)
table(predictionsR111Logistic)
table(predictionsR111Logistic, test.labels111)
qplot(predictionsR111Logistic,test.labels111, main = "Error types of Logistic111") + geom_abline(slope = 1, col = 4)


########################
######### R2 ###########
########################

R213 <- cbind(R2[,colnames(R2) %in% colnames(R2vars13)], R2[,colnames(R2) %in% colnames(datacategoric)])

inTrainingR213 <- createDataPartition(R213$ReadmissioN2, p = .75, list = FALSE)
trainingR213 <- R213[ inTrainingR213,]
testingR213 <- na.omit(R213[-inTrainingR213,])
test.labels213 <- testingR213[,14]
testingR213 <- testingR213[,-14]

nzvresultR213 <- nzv(trainingR213, saveMetrics = TRUE) # store the results for nzv

zerovarR213 <- row.names(nzvresultR213[nzvresultR213$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR213nzv <- trainingR213[,!colnames(trainingR213) %in% zerovarR213] # keep only the columns whose names are different from those in the zero variance object


rfFit1R213 <- train(trainingR213nzv$ReadmissioN2 ~ ., data = trainingR213nzv,
                    method = "ranger",
                    trControl = fitControlR2,
                    verbose = TRUE)

# With the same seed we reach an accuracy of 69.62%
predictionsR213rf <- predict(rfFit1R213, newdata = testingR213)
sum(predictionsR213rf == test.labels213)/length(test.labels213)
table(test.labels213)
table(predictionsR213rf)
table(predictionsR213rf, test.labels213)
qplot(predictionsR213rf,test.labels213, main = "Error types of RF213") + geom_abline(slope = 1, col = 4)


LogisticFit1R213 <- train(trainingR213nzv$ReadmissioN2 ~ ., data = trainingR213nzv,
                          method = "LogitBoost",
                          trControl = fitControlR2,
                          verbose = TRUE)

# We reach an accuracy of 67.09%
predictionsR213Logistic <- predict(LogisticFit1R213, newdata = testingR213)
sum(predictionsR213Logistic == test.labels213)/length(test.labels213)
table(test.labels213)
table(predictionsR213Logistic)
table(predictionsR213Logistic, test.labels213)
qplot(predictionsR213Logistic,test.labels213, main = "Error types of Logistic213") + geom_abline(slope = 1, col = 4)


########################
######### R3 ###########
########################


R312 <- cbind(R3[,colnames(R3) %in% colnames(R3vars12)], R3[,colnames(R3) %in% colnames(datacategoric)])
R312 <- R312[,-6]

inTrainingR312 <- createDataPartition(R312$ReadmissioN3, p = .75, list = FALSE)
trainingR312 <- R312[ inTrainingR312,]
testingR312 <- na.omit(R312[-inTrainingR312,])
test.labels312 <- testingR312[,12]
testingR312 <- testingR312[,-12]

nzvresultR312 <- nzv(trainingR312, saveMetrics = TRUE) # store the results for nzv

zerovarR312 <- row.names(nzvresultR312[nzvresultR312$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR312nzv <- trainingR312[,!colnames(trainingR312) %in% zerovarR312] # keep only the columns whose names are different from those in the zero variance object

rfFit1R312 <- train(trainingR312nzv$ReadmissioN3 ~ ., data = trainingR312nzv,
                    method = "ranger",
                    trControl = fitControlR3,
                    verbose = TRUE)

# We reach an accuracy of %
predictionsR312rf <- predict(rfFit1R312, newdata = testingR312)
sum(predictionsR312rf == test.labels312)/length(test.labels312)
table(test.labels312)
table(predictionsR312rf)
table(predictionsR312rf, test.labels312)
qplot(predictionsR312rf,test.labels312, main = "Error types of RF312") + geom_abline(slope = 1, col = 4)


LogisticFit1R312 <- train(trainingR312nzv$ReadmissioN3 ~ ., data = trainingR312nzv,
                          method = "LogitBoost",
                          trControl = fitControlR3,
                          verbose = TRUE)

# We reach an accuracy of %
predictionsR312Logistic <- predict(LogisticFit1R312, newdata = testingR312)
sum(predictionsR312Logistic == test.labels312)/length(test.labels312)
table(test.labels312)
table(predictionsR312Logistic)
table(predictionsR312Logistic, test.labels312)
qplot(predictionsR312Logistic,test.labels312, main = "Error types of Logistic312") + geom_abline(slope = 1, col = 4)


##############################################################################################################
##############################################################################################################



###################################################
################### MLR ###########################
###################################################

train.task <- makeClassifTask(data=trainingR1nzv, target = 'ReadmissioN1', positive = '1')
test.task <- makeClassifTask(data=testingR1nzv, target = 'ReadmissioN1', positive = '1')

lrn.logistic.R1 <- makeLearner("classif.logreg", predict.type = "prob")

LogisticR1mlr <- train.fun(lrn.logistic.R1, train.task, test.task)

train.fun <- function(learner, train.task, test.task){
  fit = mlr::train(learner, train.task)
  train.pred = predict(fit, task = train.task)
  
  test.pred = predict(fit, task = test.task)
  
  return(list(train.pred = train.pred, test.pred = test.pred))
}

model.results <- function(model){
  
  train.pred = model$train.pred
  train.acc = mlr::performance(train.pred,acc)
  train.auc = mlr::performance(train.pred,auc)
  train.ppv = mlr::performance(train.pred,ppv)
  train.brier = mlr::performance(train.pred,brier)
  train.matrix = getConfMatrix(train.pred)
  
  test.pred = model$test.pred
  test.acc = mlr::performance(test.pred,acc)
  test.auc = mlr::performance(test.pred,auc)
  test.ppv = mlr::performance(test.pred,ppv)
  test.brier = mlr::performance(test.pred,brier)
  test.matrix = getConfMatrix(test.pred)
  
  return(list(train.metrics = c(train.acc,train.auc,train.ppv,train.brier), 
              test.metrics = c(test.acc,test.auc,test.ppv,test.brier),
              train.matrix = train.matrix,test.matrix = test.matrix))
}