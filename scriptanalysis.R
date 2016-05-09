# Setting the working directory
setwd("~/Escritorio/Data Science/Masters_Project")

# Loading the data and changing possible blanks for NA
data <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

# Loading packages
library(caret)
library(randomForest)
library(splines)
library(plyr)
library(parallel)
library(gbm)
library(corrplot)
library(numDeriv)

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
R1 <- data[,-c(4,180,181)] # The response variable is the 178th column
R2 <- data[,-c(4,179,181)] # The response variable is the 178th column
R3 <- data[,-c(4,179,180)] # The response variable is the 178th column

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
testingLoS <- LoS[-inTrainingLoS,]

# Trying to solve for factor variables with only 1 factor that cause trouble in the model

nzvresult <- nzv(trainingLoS, saveMetrics = TRUE) # store the results for nzv

zerovar <- row.names(nzvresult[nzvresult$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingLoSnzv <- trainingLoS[,!colnames(trainingLoS) %in% zerovar] # keep only the columns whose names are different from those in the zero variance object

# Fitting the model gbm

fitControlLoS <- trainControl(## 5-fold CV
  method = "cv",
  number = 5)

set.seed(825)
gbmFit1LoS <- train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
                    method = "gbm",
                    trControl = fitControlLoS,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    verbose = TRUE)


# Why does predictionLoS and testingLoS have different length, how can I get a discrete prediction?
predictionsLoS <- predict(gbmFit1LoS, newdata = testingLoS)
accuracyLoS <- (predictionsLoS-testingLoS$DuradaEstada)/(length(predictionsLoS))


# Trying a rf

fitControlLoS1 <- trainControl(## 5-fold CV
  method = "cv",
  number = 3, repeats = 1)

set.seed(826)
rfFit1LoS <- train(trainingLoSnzv$DuradaEstada ~ ., data = trainingLoSnzv,
                    method = "rf",
                    trControl = fitControlLoS1,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    verbose = TRUE)

########################
######### R1 ###########
########################

inTrainingR1 <- createDataPartition(R1$ReadmissioN1, p = .75, list = FALSE)
trainingR1 <- R1[ inTrainingR1,]
testingR1 <- R1[-inTrainingR1,]

nzvresultR1 <- nzv(trainingR1, saveMetrics = TRUE) # store the results for nzv

zerovarR1 <- row.names(nzvresultR1[nzvresultR1$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR1nzv <- trainingR1[,!colnames(trainingR1) %in% zerovarR1] # keep only the columns whose names are different from those in the zero variance object

# Fitting the model gbm

fitControlR1 <- trainControl(## 5-fold CV
  method = "cv",
  number = 3, repeats = 1)

set.seed(925)
gbmFit1R1 <- train(trainingR1nzv$ReadmissioN1 ~ ., data = trainingR1nzv,
                    method = "gbm",
                    trControl = fitControlLoS,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    verbose = TRUE)




########################
######### R2 ###########
########################

inTrainingR2 <- createDataPartition(R2$ReadmissioN2, p = .75, list = FALSE)
trainingR2 <- R2[ inTrainingR2,]
testingR2 <- R2[-inTrainingR2,]


########################
######### R3 ###########
########################

inTrainingR3 <- createDataPartition(R3$ReadmissioN3, p = .75, list = FALSE)
trainingR3 <- R3[ inTrainingR3,]
testingR3 <- R3[-inTrainingR3,]
