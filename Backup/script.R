# Setting the working directory
setwd("~/Escritorio/Data Science/Masters_Project")

# Loading necessary libraries

library(corrplot)

library(ggbiplot)

library(ggplot2)

library(ggmap)

# Loading the data
data <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions160315.csv", header = TRUE)

# Changing possible blanks for NA
data <- read.csv("~/Escritorio/Data Science/Masters_Project/readmissions160315.csv", header=T, na.strings=c(""," ","NA"))

data[data==""] <- NA

data <- na.omit(data)

# Check for how many NA are in the dataset
sum(is.na(data))

head(which(is.na(data), arr.ind = T)) # See in which column and row are the NA

# Check that NumLab and NumProc are the same column

all(data$NumLab == data$NumProc)


# Changing Y/N to 1/2

# data[,43] <- as.numeric(data[,43])

# Create a function to change Yes/No to binary although...

###################################
########## WATCH OUT!!! ###########
###################################

# it actually converts from Y/N to 0/1 but it doesn't take into account the Y/N it takes into account the first value on the dataset regarding how you name it inside the function

character.to.binary <- function(x){
  switch(x,
         N = 0,
         Y  = 1,
         NA)
}

character.to.binary.gender <- function(x){
  switch(x,
         D = 0,
         H  = 1,
         NA)
}

data$CondClinEspec <- sapply(data$CondClinEspec, character.to.binary)
data$MalaltiaMental <- sapply(data$MalaltiaMental, character.to.binary)
data$AlcoholDrogues <- sapply(data$AlcoholDrogues, character.to.binary)
data$Neoplasia <- sapply(data$Neoplasia, character.to.binary)
data$RetardMental <- sapply(data$RetardMental, character.to.binary)

# fixing a colname bug
colnames(data)[18] <- "MalaltiaRespiratoria"

data$MalaltiaRespiratoria <- sapply(data$MalaltiaRespiratoria, character.to.binary)
data$Diabetis <- sapply(data$Diabetis, character.to.binary)
data$IC <- sapply(data$IC, character.to.binary)
data$PerduaPes <- sapply(data$PerduaPes, character.to.binary)
data$Depressio <- sapply(data$Depressio, character.to.binary)
data$Anemia <- sapply(data$Anemia, character.to.binary)
data$PCC <- sapply(data$PCC, character.to.binary)
data$MACA <- sapply(data$MACA, character.to.binary)
data$ServeiOnco <- sapply(data$ServeiOnco, character.to.binary)
data$ReadmissioN1 <- sapply(data$ReadmissioN1, character.to.binary)
data$ReadmissioN2 <- sapply(data$ReadmissioN2, character.to.binary)
data$Sexe <- sapply(data$Sexe, character.to.binary.gender)


# Dataframe amb predictius per DuradaEstada. La variable esta a la columna 32

which(colnames(data) == "DuradaEstada") # Check column number for response variable

datadurada <- data[,1:41]

# Dataframe amb predictius per ReadmissioN1. La variable esta a la columna 42

datar1 <- data[,c(1:31,33:42)]

# Dataframe amb predictius per ReadmissioN2. La variable esta a la columna 43

datar2 <- data[,c(1:31, 33:41, 43)]

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
# cut the length of stay variable in breaks and plot a "histogram"
data$interval <- cut(data[,32],breaks = c(seq(0,10,5),20, 70, 130))
spineplot(x=as.factor(data[,12]), y=data$interval, col= cm.colors(5,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Length of stay")

spineplot(x=as.factor(data[,12]), y=as.factor(data$ReadmissioN2), col= cm.colors(2,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Type2 Readmission")

spineplot(x=as.factor(data[,12]), y=as.factor(data$ReadmissioN1), col= cm.colors(2,alpha=0.8) ,xlab="Num. Comorbidities", ylab="Type1 Readmission")

spineplot(x=as.factor(data[,42]), y=data$interval, col= cm.colors(5,alpha=0.8) ,xlab="Type1 Readmission", ylab="Length of stay")
spineplot(x=as.factor(data[,43]), y=data$interval, col= cm.colors(5,alpha=0.8) ,xlab="Type2 Readmission", ylab="Length of stay")


#######################################
######## BARPLOTS #####################
#######################################

#### scater plot for age vs length ####


ggplot(data, aes(x=data[,1], y=data[,32], color=data[,32])) + geom_point()+ 
  scale_color_gradientn(colors=rainbow(10, start = 0.2, end = 1)) + 
  theme_minimal() + labs(x = "Age", y = "Length of stay", main = "Continuous Variables") + theme(legend.title=element_blank())

ggplot(data, aes(x=data[,1], y=data[,42], color=data[,42])) + geom_point()+ 
  scale_color_gradientn(colors=rainbow(2, start = 0.2, end = 1)) + 
  theme_minimal() + labs(x = "Age", y = "Type1 Readmission", main = "Continuous Variables") + theme(legend.title=element_blank())

ggplot(data, aes(x=data[,1], y=data[,43], color=data[,43])) + geom_point()+ 
  scale_color_gradientn(colors=rainbow(2, start = 0.2, end = 1)) +
  theme_minimal() + labs(x = "Age", y = "Type2 Readmission", main = "Continuous Variables") + theme(legend.title=element_blank())


####################################
########### CORRELATION ############
####################################

# Initial code for correlation analysis

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white", "black")

# Correlation Analysis with all numeric variables (continuos and binary), we exclude var 36 because is the same as 37

numericdata <- data[,c(1:2,12:25,28,31:32,34,37:43)]

numericdata <- as.data.frame(numericdata)

all.correlation <- cor(numericdata)

corrplot(all.correlation, method="circle")


###################################
########### HISTOGRAMS ############
###################################

# Cobertura

df_cobertura <- data.frame(patients = tail(sort(table(data$Cobertura)), 10))

df_cobertura$ins <- factor(row.names(df), levels=row.names(df))

qplot(x = df$ins, y = df$patients, xlab = "Insurance company", ylab = "Number of patients", 
      main = "Most common insurances") + theme_grey() + 
  theme(axis.text.x = element_text(angle = 70, hjust = 0.65, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


# Població

df_poblacio <- data.frame(patients = tail(sort(table(data$Poblacio)), 10))

row.names(df_poblacio) <- c("St. Cugat", "Cornella", "Sabadell", "Ripollet", "Sta. Coloma", "St. Adria", "Cerdanyola", "Badalona", "Hospitalet", "Barcelona")

df_poblacio$city <- factor(row.names(df_poblacio), levels=row.names(df_poblacio))

qplot(x = df_poblacio$city, y = df_poblacio$patients, xlab = "City", ylab = "Number of patients", 
      main = "Most common cities") + theme_grey() + theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


# GrupDiag

df_grupdiag <- data.frame(patients = tail(sort(table(data$GrupDiag)), 10))

df_grupdiag$grupdiag <- factor(row.names(df_grupdiag), levels=row.names(df_grupdiag))

qplot(x = df_grupdiag$grupdiag, y = df_grupdiag$patients, xlab = "Diagnostic Group", ylab = "Number of patients", 
      main = "Most common diagnosis") + theme_grey() + theme(axis.text.x = element_text(angle = 70, hjust = 0.65, size = 12), axis.title=element_text(size=14,face="bold")) +
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


# Servei

df_servei <- data.frame(patients = tail(sort(table(data$Servei)), 10))

df_servei$servei <- factor(row.names(df_servei), levels=row.names(df_servei))

qplot(x = df_servei$servei, y = df_servei$patients, xlab = "Service", ylab = "Number of patients", 
      main = "Most common service") + theme_grey() + theme(axis.text.x = element_text(angle = 70, hjust = 0.45, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


# OrigenAdmissio

df_origenadm <- data.frame(patients = tail(sort(table(data$OrigenAdmissio)), 5))

row.names(df_origenadm) <- c("Otros", "CAM amb ingres", "Urgente", "Normal", "C.M.A")

df_origenadm$origen <- factor(row.names(df_origenadm), levels=row.names(df_origenadm))

qplot(x = df_origenadm$origen, y = df_origenadm$patients, xlab = "Origen", ylab = "Number of patients", 
      main = "Most common origen") + theme_grey() + theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12), axis.title=element_text(size=14,face="bold"))+
  theme(panel.background = element_rect(fill = 'moccasin', colour = 'black'))


####################### MAP


map <- invisible(get_map(location = "spain", zoom = 3, maptype = "toner-lite", source = 'google'))

#get a map of USA from google source
map.final    <- ggmap(map) +
  geom_point(aes(x = geocode(data$Poblacio)[1], y = geocode(data$Poblacio)[2]),
             data = map_matrix1, colour=pers.blue, alpha=0.7) + theme_nothing()

myLocation <- as.character(unique(data$Poblacio))


location_lonlat <- matrix(nrow = )

for(i in 1:406){location_lonlat <- geocode(myLocation[i], output = c("latlon"))}


uniquelocation <- as.factor(as.data.frame(unique(data$Poblacio)))
lonlat <- geocode(c(myLocation))

location <- read.csv("~/Escritorio/Data Science/Masters_Project/foo.csv", header = TRUE)

location <- as.factor(location)

#######################################
############# PCA #####################
#######################################


# NOT GOOD RESULTS --> TRY PKG HOAMS
# subseting columns for normal pca target variable [,32]

data.durada <- data[,c(1, 12, 28, 31, 32)]

data.durada <- na.omit(data.durada) # Takes out rows with missing values

data.durada <- data.durada[complete.cases(data.durada),] # Takes out rows with missing values

data.pca.durada <- prcomp(data.durada[,-5], scale. = TRUE)

colorfunc <- colorRampPalette(c("gold","blue"))


# Plotting PCA

g <- ggbiplot(data.pca.durada, obs.scale = 0.1, var.scale = 1, alpha = 0.9, groups = as.factor(data.durada[, 5]),
              varname.size = 5, varname.adjust = 1.5, varname.abbrev = T)
g <- g + theme(legend.direction = 'vertical', legend.position = 'right')
g <- g + coord_cartesian(xlim=c(-5, 2.5), ylim=c(-4.5, 4.5))
g <- g + scale_color_manual(values=colorfunc(89)) + theme_linedraw()

print(g)


# subseting columns for normal pca target variable [,42]

data.r1 <- data[,c(1, 12, 28, 31, 42)]

data.r1 <- na.omit(data.r1)

data.pca.r1 <- prcomp(data.r1[,-5], scale. = TRUE)

# Ploting PCA for type1 readmission

g <- ggbiplot(data.pca.r1, obs.scale = 0.1, var.scale = 1, alpha = 0.2,
              groups = as.factor(data.r1[,5]), varname.size = 5, varname.adjust = 1.2, varname.abbrev = TRUE)
g <- g + theme(legend.direction = 'vertical', legend.position = 'right')
g <- g + coord_cartesian(xlim=c(-4,3), ylim=c(-4,4))
g <- g + scale_colour_manual(values = c("darkblue", "red")) + theme_linedraw()
print(g)


# subseting columns for normal pca target variable [,43]

data.r2 <- data[,c(1, 12, 28, 31, 43)]

data.r2 <- na.omit(data.r2)

data.pca.r2 <- prcomp(data.r2[,-5], scale. = TRUE)

# Ploting PCA for type2 readmission

g <- ggbiplot(data.pca.r2, obs.scale = 0.1, var.scale = 1, alpha = 0.2,
              groups = as.factor(data.r2[,5]), varname.size = 5, varname.adjust = 1.2, varname.abbrev = TRUE)
g <- g + theme(legend.direction = 'vertical', legend.position = 'right')
g <- g + coord_cartesian(xlim=c(-4,3.5), ylim=c(-4,4))
g <- g + scale_colour_manual(values = c("darkblue", "red")) + theme_linedraw()
print(g)


#########################################
############### BOXPLOT #################
#########################################

data$interval.age <- cut(data[,1], breaks = c(seq(-1,20,22), 21, 50, 70, 130), labels = c("Child and Teenagers", "Adult", "Mature", "Senior"))
ggplot(data, aes(x = as.factor(data$Sexe), y = data[,32])) + geom_boxplot() +
  facet_wrap(~interval.age)

p <- ggplot(data, aes(data$Sexe, data$DuradaEstada, group= data[,45])) + geom_boxplot()