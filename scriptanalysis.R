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
library(ROCR)
library(C50)
library(glmnet)

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

data <- data[,-c(12,14:19)] # Deletion of the categorical variables with too many categories causing issues
data <- data[,-10] # Deletion of DiesReadmissio variable


##################################################################################################
############################## VARIABLE GRUPDIAG TRANSFORMATION ##################################
##################################################################################################

unique(data$GrupDiag)
newdiaggrup <- substr(data$GrupDiag,1,2)
unique(newdiaggrup)


newdiaggrup <- replace(newdiaggrup, newdiaggrup == "NO", NA)
newdiaggrup <- replace(newdiaggrup, newdiaggrup == "VB", NA)
newdiaggrup <- replace(newdiaggrup, newdiaggrup == "VF", NA)
data$GrupDiag <- as.factor(newdiaggrup)
data <- data[,-c(158,163)] # Take out HoraIngres, HoraAlta
data <- na.omit(data)

##################################################################################################
############################## OUTLIERS DETECTION AND ELIMINATION ################################
##################################################################################################

# We are going to discard the observations with values more extreme than 0.1% of the data

outliers <- unique(c(which(data$NumMedAlta > (quantile(data$NumMedAlta, probs = 0.999))),
which(data$NumProc > (quantile(data$NumProc, probs = 0.999))),
which(data$NumLab > (quantile(data$NumLab, probs = 0.999))),
which(data$NumAdmissionsAny > (quantile(data$NumAdmissionsAny, probs = 0.999))),
which(data$NumUrgenciesAny > (quantile(data$NumUrgenciesAny, probs = 0.999))),
which(data$NumVisitesAny > (quantile(data$NumVisitesAny, probs = 0.999))),
which(na.omit(data$DuradaEstada) > (quantile(data$DuradaEstada, probs = 0.999)))))

datanO <- data[-outliers,]


# Build a matrix for each response (Length of Stay (LoS), Readmission Type 1 (R1), Readmission Type 2 (R2), Readmission Type 3 (R3))
LoS <- data[,-c(169,170,171)] # The response variable is the 4th column
LoS <- LoS[-17383,] # Eliminate the row 17383 because it has a LoS of -1
LoS$DuradaEstada <- cut(LoS[,4], breaks = c(-1, 2, 7, 133), labels = c("Less than 2 days", "Between 3 to 7 days", "More than 1 week")) # Transform the LoS variable into factor intervals labeled

R1 <- data[,-c(4,170,171)] # The response variable is the 169th column
R2 <- data[,-c(4,169,171)] # The response variable is the 170th column
R2$PeriodeIngres <- as.factor(as.character(R2$PeriodeIngres))
R3 <- data[,-c(4,169,170)] # The response variable is the 170th column

LoSnO <- datanO[,-c(169,170,171)] 
LoSnO <- LoSnO[-17383,]
LoSnO$DuradaEstada <- cut(LoSnO[,4], breaks = c(-1, 2, 7, 133), labels = c("Less than 2 days", "Between 3 to 7 days", "More than 1 week"))

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

gbmFit1LoS <- caret::train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
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
table(predictionsLoSgbm, test.labels)
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


glmnetFit1LoS <- train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
                   method = "glmnet",
                   trControl = fitControlLoS)

predictionsLoSglmnet <- predict(glmnetFit1LoS, newdata = testingLoS)
table(predictionsLoSglmnet, test.labels)



#################################################
######### LENGTH OF STAY W/O OUTLIERS ###########
#################################################

LoSnO <- LoSnO[-c(which(is.na(LoSnO[,4]), arr.ind = T)),]

inTrainingLoSnO <- createDataPartition(LoSnO$DuradaEstada, p = .75, list = FALSE)
trainingLoSnO <- LoSnO[ inTrainingLoSnO,]
testingLoSnO <- na.omit(LoSnO[-inTrainingLoSnO,])
test.labels <- testingLoSnO[,4]
testingLoSnO <- testingLoSnO[,-4]

nzvresult <- nzv(trainingLoSnO, saveMetrics = TRUE) # store the results for nzv

zerovar <- row.names(nzvresult[nzvresult$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingLoSnOnzv <- trainingLoSnO[,!colnames(trainingLoSnO) %in% zerovar] # keep only the columns whose names are different from those in the zero variance object

fitControlLoSnO <- trainControl(## 5-fold CV
  method = "cv",
  number = 5, repeats = 1)

gbmFit1LoSnO <- caret::train(trainingLoSnOnzv$DuradaEstada ~ ., data = trainingLoSnOnzv,
                           method = "gbm",
                           trControl = fitControlLoSnO,
                           verbose = TRUE)

predictionsLoSnOgbm <- predict(gbmFit1LoSnO, newdata = testingLoSnO)
table(predictionsLoSnOgbm, test.labels)


rfFit1LoSnO <- train(trainingLoSnOnzv$DuradaEstada ~ ., data = trainingLoSnOnzv,
                   method = "ranger",
                   trControl = fitControlLoSnO)

predictionsLoSnOrf <- predict(rfFit1LoSnO, newdata = testingLoSnO)
table(predictionsLoSnOrf, test.labels)



glmnetFit1LoSnO <- train(trainingLoSnOnzv$DuradaEstada ~ ., data = trainingLoSnOnzv,
                       method = "glmnet",
                       trControl = fitControlLoSnO)

predictionsLoSnOglmnet <- predict(glmnetFit1LoSnO, newdata = testingLoSnO)
table(predictionsLoSnOglmnet, test.labels)


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

nzvresultR1 <- nzv(R1, saveMetrics = TRUE) # store the results for nzv

zerovarR1 <- row.names(nzvresultR1[nzvresultR1$nzv == TRUE,]) # subset only the variables with zero variance
zerovarR1 <- zerovarR1[!zerovarR1 %in% "ReadmissioN1"]

trainingR1nzv <- trainingR1[,!colnames(trainingR1) %in% zerovarR1]
testingR1nzv <- testingR1[,!colnames(testingR1) %in% zerovarR1]# keep only the columns whose names are different from those in the zero variance object

trainingR1nzv <- trainingR1nzv[,-c(136,138)]
testingR1nzv <- testingR1nzv[,-c(136,138)]


########################
######### R2 ###########
########################

# Trying a rf with all the variables for R2 to see what's going on

R2$ReadmissioN2 <- as.character(R2$ReadmissioN2)
R2$ReadmissioN2[R2$ReadmissioN2 == 'Y'] <- 1
R2$ReadmissioN2[R2$ReadmissioN2 == 'N'] <- 0
R2$ReadmissioN2 <- as.factor(R2$ReadmissioN2)
inTrainingR2 <- createDataPartition(R2$ReadmissioN2, p = .75, list = FALSE)
trainingR2 <- R2[ inTrainingR2,]
testingR2 <- na.omit(R2[-inTrainingR2,])

nzvresultR2 <- nzv(trainingR2, saveMetrics = TRUE) # store the results for nzv

zerovarR2 <- row.names(nzvresultR2[nzvresultR2$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR2nzv <- trainingR2[,!colnames(trainingR2) %in% zerovarR2] # keep only the columns whose names are different from those in the zero variance object
testingR2nzv <- testingR2[,!colnames(testingR2) %in% zerovarR2]

trainingR2nzv <- trainingR2nzv[,-c(135,137)]
testingR2nzv <- testingR2nzv[,-c(135,137)]

########################
######### R3 ###########
########################

# Trying a rf with all the variables for R3 to see what's going on

R3$ReadmissioN3 <- as.character(R3$ReadmissioN3)
R3$ReadmissioN3[R3$ReadmissioN3 == 'Y'] <- 1
R3$ReadmissioN3[R3$ReadmissioN3 == 'N'] <- 0
R3$ReadmissioN3 <- as.factor(R3$ReadmissioN3)
inTrainingR3 <- createDataPartition(R3$ReadmissioN3, p = .75, list = FALSE)
trainingR3 <- R3[ inTrainingR3,]
testingR3 <- na.omit(R3[-inTrainingR3,])
#test.labels3 <- testingR3[,168]
#testingR3 <- testingR3[,-168]

nzvresultR3 <- nzv(trainingR3, saveMetrics = TRUE) # store the results for nzv

zerovarR3 <- row.names(nzvresultR3[nzvresultR3$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR3nzv <- trainingR3[,!colnames(trainingR3) %in% zerovarR3] # keep only the columns whose names are different from those in the zero variance object
testingR3nzv <- testingR3[,!colnames(testingR3) %in% zerovarR3]

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


##############################################################################################################
##############################################################################################################



###################################################
################### MLR ###########################
###################################################

############### R1

trainingR1nzv = trainingR1 # Use after selecting the significant variables only
testingR1nzv = testingR1 # Use after selecting the significant variables only

train.task <- makeClassifTask(data=trainingR1nzv, target = 'ReadmissioN1', positive = '1')
test.task <- makeClassifTask(data=testingR1nzv, target = 'ReadmissioN1', positive = '1')

lrn.logistic.R1 <- makeLearner("classif.logreg", predict.type = "prob")

LogisticR1mlr <- train.fun(lrn.logistic.R1, train.task, test.task)

train.fun <- function(learner, train.task, test.task){
  fit = mlr::train(learner, train.task)
  train.pred = predict(fit, task = train.task)
  
  d <- generateThreshVsPerfData(train.pred, measures = list(fpr, fnr, f1))
  df = d$data
  best.thres = df[order(df$f1, decreasing = TRUE),4]

  test.pred = setThreshold(predict(fit, task = test.task), best.thres[1])
  
  #test.pred = predict(fit, task = test.task)
  
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

 # f1 = harmonic mean of ppv and tpr
d <- generateThreshVsPerfData(LogisticR1mlr$test.pred, measures = list(fpr, fnr, f1))
df = d$data
df = df[order(df$f1, decreasing = T),]
plotThreshVsPerf(d)

c <- generateThreshVsPerfData(LogisticR1mlr$test.pred, measures = list(fpr, tpr))
plotROCCurves(c)

p <- generateThreshVsPerfData(LogisticR1mlr$test.pred, measures = list(ppv, tpr, tnr))
plotROCCurves(p, measures = list(tpr, ppv), diagonal = FALSE)


# glmnet regression with cv.glmnet - elastic net

lrn <- makeLearner('classif.cvglmnet', predict.type = 'prob')
rdesc <- makeResampleDesc("CV", iters = 10)
ctrl <- makeTuneControlGrid()
ps <- makeParamSet(makeDiscreteParam("alpha", values = seq(0.5,1,0.1)),
                   makeDiscreteParam("s", values = c("lambda.min")))
tuning = tuneParams("classif.cvglmnet", task = train.task, resampling = rdesc, par.set = ps,
                    control = ctrl,measures = f1)

lrn.cvglmnet = setHyperPars(makeLearner("classif.cvglmnet", predict.type = 'prob'), par.vals = tuning$x)

cvglmnet = train.fun(lrn.cvglmnet,train.task,test.task)
res.cvglmnet = model.results(cvglmnet)



############### R2

trainingR2nzv = trainingR2 # Use after selecting the significant variables only
testingR2nzv = testingR2 # Use after selecting the significant variables only

train.task2 <- makeClassifTask(data=trainingR2nzv, target = 'ReadmissioN2', positive = '1')
test.task2 <- makeClassifTask(data=testingR2nzv, target = 'ReadmissioN2', positive = '1')

lrn.logistic.R2 <- makeLearner("classif.logreg", predict.type = "prob")

LogisticR2mlr <- train.fun(lrn.logistic.R2, train.task2, test.task2)

train.fun <- function(learner, train.task, test.task){
  fit = mlr::train(learner, train.task)
  train.pred = predict(fit, task = train.task)
  
    d <- generateThreshVsPerfData(train.pred, measures = list(fpr, fnr, f1))
    df = d$data
    best.thres = df[order(df$f1, decreasing = TRUE),4]
  
    test.pred = setThreshold(predict(fit, task = test.task), best.thres[1])
  
  #test.pred = predict(fit, task = test.task)
  
  return(list(train.pred = train.pred, test.pred = test.pred))
}

d2 <- generateThreshVsPerfData(LogisticR2mlr$test.pred, measures = list(fpr, fnr, mmce))
df2 = d2$data
df2 = df2[order(df$f1),]
plotThreshVsPerf(d2)

c2 <- generateThreshVsPerfData(LogisticR2mlr$test.pred, measures = list(fpr, tpr))
plotROCCurves(c2)

p2 <- generateThreshVsPerfData(LogisticR2mlr$test.pred, measures = list(ppv, tpr, tnr))
plotROCCurves(p2, measures = list(tpr, ppv), diagonal = FALSE)


#glmnet regression with cv.glmnet - elastic net

ps2 <- makeParamSet(makeDiscreteParam("alpha", values = seq(0,1,0.1)),
                   makeDiscreteParam("s", values = c("lambda.min")))
tuning = tuneParams("classif.cvglmnet", task = train.task2, resampling = rdesc, par.set = ps2,
                    control = ctrl, measures = f1)

lrn.cvglmnet2 = setHyperPars(makeLearner("classif.cvglmnet", predict.type = 'prob'), par.vals = tuning$x)

cvglmnet2 = train.fun(lrn.cvglmnet2,train.task2,test.task2)
res.cvglmnet2 = model.results(cvglmnet2)



############### R3

trainingR3nzv = trainingR3 # Use after selecting the significant variables only
testingR3nzv = testingR3 # Use after selecting the significant variables only


train.task3 <- makeClassifTask(data=trainingR3nzv, target = 'ReadmissioN3', positive = '1')
test.task3 <- makeClassifTask(data=testingR3nzv, target = 'ReadmissioN3', positive = '1')

lrn.logistic.R3 <- makeLearner("classif.logreg", predict.type = "prob")

LogisticR3mlr <- train.fun(lrn.logistic.R3, train.task, test.task)

train.fun <- function(learner, train.task, test.task){
  fit = mlr::train(learner, train.task)
  train.pred = predict(fit, task = train.task)
  
  d <- generateThreshVsPerfData(train.pred, measures = list(fpr, fnr, f1))
  df = d$data
  best.thres = df[order(df$f1, decreasing = TRUE),4]
  
  test.pred = setThreshold(predict(fit, task = test.task), best.thres[1])
  
  #test.pred = predict(fit, task = test.task)
  
  return(list(train.pred = train.pred, test.pred = test.pred))
}

d3 <- generateThreshVsPerfData(LogisticR3mlr$test.pred, measures = list(fpr, fnr, mmce))
df3 = d3$data
df3 = df3[order(df$mmce),]
plotThreshVsPerf(d3)

c3 <- generateThreshVsPerfData(LogisticR3mlr$test.pred, measures = list(fpr, tpr))
plotROCCurves(c3)

p3 <- generateThreshVsPerfData(LogisticR3mlr$test.pred, measures = list(ppv, tpr, tnr))
plotROCCurves(p3, measures = list(tpr, ppv), diagonal = FALSE)

#glmnet regression with cv.glmnet - elastic net

ps3 <- makeParamSet(makeDiscreteParam("alpha", values = seq(0,1,0.1)),
                    makeDiscreteParam("s", values = c("lambda.min")))
tuning = tuneParams("classif.cvglmnet", task = train.task3, resampling = rdesc, par.set = ps3,
                    control = ctrl, measures = f1)

lrn.cvglmnet3 = setHyperPars(makeLearner("classif.cvglmnet", predict.type = 'prob'), par.vals = tuning$x)

cvglmnet3 = train.fun(lrn.cvglmnet3,train.task3,test.task3)
res.cvglmnet3 = model.results(cvglmnet3)

