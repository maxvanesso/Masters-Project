# Setting the working directory
setwd("~/Escritorio/Data Science/Masters_Project")

library(corrplot)

library(ggbiplot)

library(ggplot2)

library(caret)

# Loading the data and changing possible blanks for NA
data <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

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


#############################################################
########################### R3=N ############################
#############################################################

# Loading the data and changing possible blanks for NA
dataRN <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)

# Fix a misspelled colname
index <- which(colnames(dataRN) == 'MalaltiaRespitatoria')
colnames(dataRN)[index] <- 'MalaltiaRespiratoria'

# Replace some values in a column
dataRN$PCC <- replace(dataRN$PCC, dataRN$PCC == "S", "Y")
dataRN$MACA <- replace(dataRN$MACA, dataRN$MACA == "S", "Y")
dataRN$ServeiOnco <- replace(dataRN$ServeiOnco, dataRN$ServeiOnco == "S", "Y")
dataRN[,13:143] <- replace(dataRN[,13:143], dataRN[,13:143] == 2, 0)

# Take out the two columns that we don't need: DataIngres and DataAlta
dataRN <- dataRN[,c(1:158, 160:164, 166:183)]

# Change the columns to factors and bind them with the non factors (discrete and continuous)
dataRN <- cbind(dataRN[,-c(2:143,145:157,159:168,170:172,179:181)],
                data.frame(apply(dataRN[,c(2:143,145:157,159:168,170:172,179:181)], 2, factor)))



# Select only the NR
dataRN <- dataRN[dataRN$ReadmissioN3 == "N",]
dataRN <- dataRN[dataRN$ReadmissioN2 == "N",]
dataRN <- dataRN[dataRN$ReadmissioN1 == "N",]

#############################################################
########################### R3=Y ############################
#############################################################

# Loading the data and changing possible blanks for NA
dataRY <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions_20160426.csv", header=T, na.strings=c(""," ","NA"), stringsAsFactors = FALSE)


# Fix a misspelled colname
index <- which(colnames(dataRY) == 'MalaltiaRespitatoria')
colnames(dataRY)[index] <- 'MalaltiaRespiratoria'

# Replace some values in a column
dataRY$PCC <- replace(dataRY$PCC, dataRY$PCC == "S", "Y")
dataRY$MACA <- replace(dataRY$MACA, dataRY$MACA == "S", "Y")
dataRY$ServeiOnco <- replace(dataRY$ServeiOnco, dataRY$ServeiOnco == "S", "Y")
dataRY[,13:143] <- replace(dataRY[,13:143], dataRY[,13:143] == 2, 0)

# Take out the two columns that we don't need: DataIngres and DataAlta
dataRY <- dataRY[,c(1:158, 160:164, 166:183)]

# Change the columns to factors and bind them with the non factors (discrete and continuous)
dataRY <- cbind(dataRY[,-c(2:143,145:157,159:168,170:172,179:181)],
              data.frame(apply(dataRY[,c(2:143,145:157,159:168,170:172,179:181)], 2, factor)))


# Select only the R3 with positive values

dataRY <- dataRY[dataRY$ReadmissioN3 == "Y"|dataRY$ReadmissioN2 == "Y"|dataRY$ReadmissioN1 == "Y",]

################################################################
################### CORRELATION ################################
################################################################

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

datanumeric <- datanumeric[datanumeric$ReadmissioN3 == 0&datanumeric$ReadmissioN2 == 0&datanumeric$ReadmissioN1 == 0,]

# We select the 150 with var > 0 if we take out the nzv as well we will end up with the 49 variables
cornzvresult <- nzv(datanumeric, saveMetrics = TRUE) # store the results for nzv

corzerovar150 <- row.names(cornzvresult[cornzvresult$zeroVar == TRUE,]) # subset only the variables with zero variance

datanumericnzv150 <- datanumeric[,!colnames(datanumeric) %in% corzerovar150] # keep only the columns whose names are different from those in the zero variance object

correlation150 <- cor(datanumericnzv150, use="pairwise.complete.obs") # if there are NA the correlation is computed only with the completed pairs of the two columns

corrplot(correlation150, method="square", tl.cex = 0.3, order = "hclust") # How to visualize it better?


##############################################################################
################ DESCRIPTIVE ANALYSIS ########################################
##############################################################################

# Personal colours

co <- 1/255 
pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.oragange   <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )


#######################################
########### SPINELOTS #################
#######################################

# divide readmission type variable into two and plot a "histogram" versus num comorbidities

spineplot(x=as.factor(data[,2]), y=as.factor(data$ReadmissioN3), col= cm.colors(2,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Type3 Readmission")

spineplot(x=as.factor(data[,2]), y=as.factor(data$ReadmissioN2), col= cm.colors(2,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Type2 Readmission")

spineplot(x=as.factor(data[,2]), y=as.factor(data$ReadmissioN1), col= cm.colors(2,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Type1 Readmission")

#######################################
######## BARPLOTS #####################
#######################################

#### scater plot for age vs readmission ####

ggplot(dataRY, aes(x=dataRY[,1], y=dataRY[,181], color=dataRY[,181])) + geom_point()+
  theme_minimal() + labs(x = "Age", y = "Type3 Readmission", main = "Continuous Variables") + theme(legend.title=element_blank())

ggplot(dataRY, aes(x=dataRY[,1], y=dataRY[,180], color=dataRY[,180])) + geom_point()+
  theme_minimal() + labs(x = "Age", y = "Type2 Readmission", main = "Continuous Variables") + theme(legend.title=element_blank())

ggplot(dataRY, aes(x=dataRY[,1], y=dataRY[,179], color=dataRY[,179])) + geom_point()+
  theme_minimal() + labs(x = "Age", y = "Type1 Readmission", main = "Continuous Variables") + theme(legend.title=element_blank())


###################################
########### HISTOGRAMS ############
###################################

histogram(dataRY$EdatEnAlta, xlab="Age", ylab="Percentage of ReadmissioY", type="density", aspect = 0.5, breaks=max(dataRY$EdatEnAlta))
histogram(dataRN$EdatEnAlta, xlab="Age", ylab="Percentage of ReadmissioN", type="density", aspect = 0.5, breaks=max(dataRN$EdatEnAlta))

histogram(dataRY$NumComorbiditats, xlab="Number of comorbidities", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumComorbiditats))
histogram(dataRN$NumComorbiditats, xlab="Number of comorbidities", ylab="Percentage of ReadmissioN", breaks=9)

histogram(dataRY$NumMedAlta, xlab="Number of drugs on discharge", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumMedAlta))
histogram(dataRN$NumMedAlta, xlab="Number of drugs on discharge", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumMedAlta))

histogram(dataRY$NumProc, xlab="Number of procedures", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumProc))
histogram(dataRN$NumProc, xlab="Number of procedures", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumProc))

histogram(dataRY$NumLab, xlab="Number of lab tests", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumLab))
histogram(dataRN$NumLab, xlab="Number of lab tests", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumLab))

histogram(dataRY$NumAdmissionsAny, xlab="Number of yearly admissions", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumAdmissionsAny))
histogram(dataRN$NumAdmissionsAny, xlab="Number of yearly admissions", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumAdmissionsAny))

histogram(dataRY$NumUrgenciesAny, xlab="Number of emergencies", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumUrgenciesAny))
histogram(dataRN$NumUrgenciesAny, xlab="Number of emergencies", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumUrgenciesAny))

histogram(dataRY$NumVisitesAny, xlab="Number of visits", ylab="Percentage of ReadmissioY", breaks=max(dataRY$NumVisitesAny))
histogram(dataRN$NumVisitesAny, xlab="Number of visits", ylab="Percentage of ReadmissioN", breaks=max(dataRN$NumVisitesAny))

histogram(dataRY$DiesReadmissio, xlab="Days until readmission", ylab="Percentage of ReadmissioY", breaks=max(dataRY$DiesReadmissio))
histogram(dataRN$DiesReadmissio, xlab="Days until readmission", ylab="Percentage of ReadmissioN", breaks=max(dataRN$DiesReadmissio))



# Cobertura

df_cobertura <- data.frame(patients = tail(sort(table(dataRY$Cobertura)), 10))

qplot(x = df_cobertura$patients.Var1, y = df_cobertura$patients.Freq/676, xlab = "Insurance company", ylab = "Number of readmitted patients", 
      main = "Most common insurances") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



df_cobertura <- data.frame(patients = tail(sort(table(dataRN$Cobertura)), 10))

qplot(x = df_cobertura$patients.Var1, y = df_cobertura$patients.Freq/18216, xlab = "Insurance company", ylab = "Number of no-readmitted patients", 
      main = "Most common insurances") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



# Població

df_poblacio <- data.frame(patients = tail(sort(table(dataRY$Poblacio)), 10))

qplot(x = df_poblacio$patients.Var1, y = df_poblacio$patients.Freq/676, xlab = "City", ylab = "Number of readmitted patients", 
      main = "Most common cities") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



df_poblacio <- data.frame(patients = tail(sort(table(dataRN$Poblacio)), 10))

qplot(x = df_poblacio$patients.Var1, y = df_poblacio$patients.Freq/18216, xlab = "City", ylab = "Number of no-readmitted patients", 
      main = "Most common cities") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



# GrupDiag

df_grupdiag <- data.frame(patients = tail(sort(table(dataRY$GrupDiag)), 10))

qplot(x = df_grupdiag$patients.Var1, y = df_grupdiag$patients.Freq/676, xlab = "Diagnostic Group", ylab = "Number of readmitted patients", 
      main = "Most common diagnosis") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



df_grupdiag <- data.frame(patients = tail(sort(table(dataRN$GrupDiag)), 10))

qplot(x = df_grupdiag$patients.Var1, y = df_grupdiag$patients.Freq/18216, xlab = "Diagnostic Group", ylab = "Number of no-readmitted patients", 
      main = "Most common diagnosis") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



# Servei

df_servei <- data.frame(patients = tail(sort(table(dataRY$Servei)), 10))

qplot(x = df_servei$patients.Var1, y = df_servei$patients.Freq/676, xlab = "Service", ylab = "Number of readmitted patients", 
      main = "Most common service") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



df_servei <- data.frame(patients = tail(sort(table(dataRN$Servei)), 10))

qplot(x = df_servei$patients.Var1, y = df_servei$patients.Freq/18216, xlab = "Service", ylab = "Number of no-readmitted patients", 
      main = "Most common service") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.95, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



# OrigenAdmissio

df_origenadm <- data.frame(patients = tail(sort(table(dataRY$OrigenAdmissio)), 5))

qplot(x = df_origenadm$patients.Var1, y = df_origenadm$patients.Freq/676, xlab = "Origen", ylab = "Number of readmitted patients", 
      main = "Most common origen") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))



df_origenadm <- data.frame(patients = tail(sort(table(dataRN$OrigenAdmissio)), 5))

qplot(x = df_origenadm$patients.Var1, y = df_origenadm$patients.Freq/18216, xlab = "Origen", ylab = "Number of no-readmitted patients", 
      main = "Most common origen") + theme_grey() + scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


##################################################################################################
##################################################################################################


### Age ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(EdatEnAlta))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=EdatEnAlta, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="EdatEnAlta histogram plot",x="EdatEnAlta (Age)", y = "Density")




### Number of comorbidities ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumComorbiditats))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumComorbiditats, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumComorbiditats histogram plot",x="NumComorbiditats (Comorbidities)", y = "Density")




### Number of drugs on discharge ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumMedAlta))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumMedAlta, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumMedAlta histogram plot",x="NumMedAlta (Number of drugs on discharge)", y = "Density")




### Number of procedures ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumProc))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumProc, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumProc histogram plot",x="NumProc (Number of Procedures)", y = "Density")





### Number of lab tests ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumLab))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumLab, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumLab histogram plot",x="NumLab (Number of lab tests)", y = "Density")





### Number of yearly admissions ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumAdmissionsAny))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumAdmissionsAny, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumAdmissionsAny histogram plot",x="NumAdmissionsAny (Yearly admissions)", y = "Density")




### Number of emergencies ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumUrgenciesAny))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumUrgenciesAny, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumUrgenciesAny histogram plot",x="NumUrgenciesAny (Emergencies)", y = "Density")




### Number of visits ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(NumVisitesAny))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=NumVisitesAny, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="NumVisitesAny histogram plot",x="NumVisitesAny (Appointments)", y = "Density")




### Days until readmission ###

mu <- ddply(datanumeric, "ReadmissioN1", summarise, grp.mean=mean(DiesReadmissio))

mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 1, "Y")
mu$ReadmissioN1 <- replace(mu$ReadmissioN1, mu$ReadmissioN1 == 0, "N")

p <- ggplot(data, aes(x=DiesReadmissio, color=ReadmissioN1)) + geom_histogram(aes(y=..density..), binwidth=1, fill="white", alpha=0.8, position = "dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color = ReadmissioN1), linetype="dashed")
p  + geom_density(alpha=.1, fill=pers.blue) + scale_color_brewer(palette = "Dark2") +
  theme_classic() + labs(title="DiesReadmissio histogram plot",x="DiesReadmissio (Days until readmission)", y = "Density")
