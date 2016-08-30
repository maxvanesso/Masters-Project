########################################################################################
################### DATA EXPLORATION AND SUMMARY OF VARIABLES ##########################
########################################################################################


summary(R1$PeriodeIngres)
summary(R1$PeriodeAlta)
which(R1$PeriodeIngres == 2014)
table(R1$PeriodeIngres)
which(colnames(data) == 'PeriodeIngres')

write.csv(R1, file = "/home/max/Escritorio/Data Science/Masters_Project/test.csv")

data[c(16472,17027,17130,17218,17330,17459,17532,17988,18065,18075,18269,18543,18677,18764), 163]

R1 <- read.csv("~/Escritorio/Data Science/Masters_Project/R1.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)


########################################################
################# NEW R1 DATASET #######################
########################################################

R1vars <- which(colnames(data) %in% c("ReadmissioN1", "PCC", "diag09G", "GrupDiag", "Poblacio2", "Sexe", "NumUrgenciesAny", "NumMedAlta", "EdatEnAlta"))
R1 <- data[,R1vars]

R1$ReadmissioN1 <- as.character(R1$ReadmissioN1)
R1$ReadmissioN1[R1$ReadmissioN1 == 'Y'] <- 1
R1$ReadmissioN1[R1$ReadmissioN1 == 'N'] <- 0
R1$ReadmissioN1 <- as.factor(R1$ReadmissioN1)
inTrainingR1 <- createDataPartition(R1$ReadmissioN1, p = .75, list = FALSE)
trainingR1 <- na.omit(R1[ inTrainingR1,])
testingR1 <- na.omit(R1[-inTrainingR1,])



########################################################
################# NEW R2 DATASET #######################
########################################################

R2vars <- which(!colnames(data) %in% c("ReadmissioN1","ReadmissioN3", "diagVI", "diagVH", "diagVG", "diag17V", "diag16C", "diag14X", "diag13D", "diag12C", "diag10F", "diag09G", "diag08F", "diag07I", "diag06I", "diag05C", "diag04Z", "diag03D", "diag02L", "diag01O"))
R2 <- data[,R2vars]

R2$ReadmissioN2 <- as.character(R2$ReadmissioN2)
R2$ReadmissioN2[R2$ReadmissioN2 == 'Y'] <- 1
R2$ReadmissioN2[R2$ReadmissioN2 == 'N'] <- 0
R2$ReadmissioN2 <- as.factor(R2$ReadmissioN2)
R2$PeriodeIngres <- as.factor(as.character(R2$PeriodeIngres))
inTrainingR2 <- createDataPartition(R2$ReadmissioN2, p = .75, list = FALSE)
trainingR2 <- na.omit(R2[ inTrainingR2,])
testingR2 <- na.omit(R2[-inTrainingR2,])

nzvresultR2 <- nzv(trainingR2, saveMetrics = TRUE) # store the results for nzv

zerovarR2 <- row.names(nzvresultR2[nzvresultR2$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR2nzv <- trainingR2[,!colnames(trainingR2) %in% zerovarR2] # keep only the columns whose names are different from those in the zero variance object
testingR2nzv <- testingR2[,!colnames(testingR2) %in% zerovarR2]

trainingR2nzv <- trainingR2nzv[,-c(115,117)] # Take out Servei and OrigenAdmissio
testingR2nzv <- testingR2nzv[,-c(115,117)]


# After running the fit we select the variables significant for the model

R2varsdef <- which(colnames(data) %in% c("ReadmissioN2", "PCC", "NumUrgenciesAny", "NumMedAlta", "NumLab", "Cobertura2", "GrupDiag", "diag01A", "diag01D", "diag07C", "diag07F", "diag08D", "MACA", "MesIngres"))
R2 <- data[,R2varsdef]

R2$ReadmissioN2 <- as.character(R2$ReadmissioN2)
R2$ReadmissioN2[R2$ReadmissioN2 == 'Y'] <- 1
R2$ReadmissioN2[R2$ReadmissioN2 == 'N'] <- 0
R2$ReadmissioN2 <- as.factor(R2$ReadmissioN2)
inTrainingR2 <- createDataPartition(R2$ReadmissioN2, p = .75, list = FALSE)
trainingR2 <- na.omit(R2[ inTrainingR2,])
testingR2 <- na.omit(R2[-inTrainingR2,])


########################################################
################# NEW R3 DATASET #######################
########################################################

R3 <- data[,-c(4,169,170)] # The response variable is the 170th column

R3$ReadmissioN3 <- as.character(R3$ReadmissioN3)
R3$ReadmissioN3[R3$ReadmissioN3 == 'Y'] <- 1
R3$ReadmissioN3[R3$ReadmissioN3 == 'N'] <- 0
R3$ReadmissioN3 <- as.factor(R3$ReadmissioN3)
R3$PeriodeIngres <- as.factor(as.character(R3$PeriodeIngres))
inTrainingR3 <- createDataPartition(R3$ReadmissioN3, p = .75, list = FALSE)
trainingR3 <- na.omit(R3[ inTrainingR3,])
testingR3 <- na.omit(R3[-inTrainingR3,])

nzvresultR3 <- nzv(trainingR3, saveMetrics = TRUE) # store the results for nzv

zerovarR3 <- row.names(nzvresultR3[nzvresultR3$zeroVar == TRUE,]) # subset only the variables with zero variance

trainingR3nzv <- trainingR3[,!colnames(trainingR3) %in% zerovarR3] # keep only the columns whose names are different from those in the zero variance object
testingR3nzv <- testingR3[,!colnames(testingR3) %in% zerovarR3]

trainingR3nzv <- trainingR3nzv[,-c(134,136)] # Take out Servei and OrigenAdmissio
testingR3nzv <- testingR3nzv[,-c(134,136)]


# After running the fit we select the variables significant for the model

R3varsdef <- which(colnames(data) %in% c("ReadmissioN3", "PCC", "NumUrgenciesAny", "NumMedAlta", "EdatEnAlta", "GrupDiag","diag09G", "Sexe"))
R3 <- data[,R3varsdef]

R3$ReadmissioN3 <- as.character(R3$ReadmissioN3)
R3$ReadmissioN3[R3$ReadmissioN3 == 'Y'] <- 1
R3$ReadmissioN3[R3$ReadmissioN3 == 'N'] <- 0
R3$ReadmissioN3 <- as.factor(R3$ReadmissioN3)
inTrainingR3 <- createDataPartition(R3$ReadmissioN3, p = .75, list = FALSE)
trainingR3 <- na.omit(R3[ inTrainingR3,])
testingR3 <- na.omit(R3[-inTrainingR3,])
