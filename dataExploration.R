#######################################
###      Data Exploration Guide     ###
#######################################

## Clear environment and set pathname
rm(list = ls())
setwd(pathname)


## Read in the data set from csv file
dataset <- read.csv("dataset.csv")

########################################
### Step 1: Variable Identification  ###
########################################
str(dataset) # Display the structure of the data set
View(dataset) # Display the dataset

# Set Outcome as factor as well
dataset$Outcome <- as.factor(dataset$Outcome)
str(dataset)

# go through each variable column from 1 to 8
# Any cell in each column containing 0 should become NA
for (colIndex in c(1:8)){ 
  dataset[which(dataset[, colIndex] == 0), colIndex] <- NA 
}

###################################
### Step 2: Univariate Analysis ###
###################################
summary(dataset) # Display a summary of the data set

library(reshape2)
library(ggplot2)

## Keeping factor variables out of this analysis as its binary variable
data_pp<-data.frame(rownumber=c(1:nrow(dataset)),dataset[,-c(9:13)])
data_pp<-reshape2::melt(data_pp,id.var="rownumber")

## Create box-plots for each variable
ggplot(data=data_pp,aes(x=variable,y=value)) +
  geom_boxplot(aes(x=variable)) +
  facet_wrap( ~ variable, scales="free")

## Create histogram for each variable
ggplot(data=data_pp,aes(x = value)) + 
  facet_wrap(~variable,scales = "free") + 
  geom_histogram()


## Creating Bargraphs for Categorical variables
cnames <- colnames(dataset)
par(mfrow=c(2, 3)) 
for(i in cnames[9:13]){
  plot(dataset[[i]],xlab = i,type = "p",col = "blue", lwd = 1 )
}

###################################
###  Step 3: Bivariate Analysis ###
###################################

## For continous variables
library(gclus)
my.abs     <- abs(cor(dataset[,-c(9:13)]))
my.colors  <- dmat.color(my.abs)
my.ordered <- order.single(cor(dataset[,-c(9:13)]))
cpairs(dataset[,-c(9:13)], my.ordered, panel.colors=my.colors, gap=0.5)


# Table for Categorical Variable
summary(dataset[,-c(1:8)])


#########################################
###  Step 4: Variable Transformations ###
#########################################

par(mfrow=c(1, 2)) 

boxplot(dataset$Insulin, main="Insulin") 
boxplot(log(dataset$Insulin), main="Log Insulin") 

boxplot(dataset$Age, main="Age") 
boxplot(log(dataset$Age), main="Log Age") 


########################################
###  Step 5: Missing Value Treatment ###
########################################

# Checking for missing values
apply(dataset, 2,function(x) round(sum(is.na(x)),2))


# Do Little's MCAR test to see if the missing values
# are completely random
library(BaylorEdPsych)
MCAR.p.value <- LittleMCAR(dataset)$p.value
cat("p.value for Little's MCAR test = ", MCAR.p.value)
# Since p < 0.05, we reject the hypothesis that values are missing
# completely random. So complete case analysis, or imputation using
# the mean would lead to bias estimations.


# Imputation option 1: Impute missing data using kNN
library(VIM)
dataset_kNN <- kNN(dataset, variable = names(dataset)[1:8], k = 7, imp_var = FALSE)

# Imputation option 2: Impute missing data using regression
library(mice)
reg_imp <- mice(dataset, m = 1, method = "norm.predict")$imp
dataset_Regression <- dataset
for (i in 1:ncol(dataset_Regression)){
  if (any(is.na(dataset_Regression[,i]))){
    dataset_Regression[rownames(reg_imp[[names(dataset_Regression)[i]]]),i] <- reg_imp[[names(dataset_Regression)[i]]]
  }
}

# Imputation option 3: Impute missing data using the mean of each variable
mean_imp <- mice(dataset, m = 1, method = "mean")$imp
dataset_mean <- dataset
for (i in 1:ncol(dataset_mean)){
  if (any(is.na(dataset_mean[,i]))){
    dataset_mean[rownames(mean_imp[[names(dataset_mean)[i]]]),i] <- mean_imp[[names(dataset_mean)[i]]]
  }
}

# Imputation option 4: Instead of imputation, drop any records with missing value
dataset_Complete_Cases <- dataset[complete.cases(dataset),]

# Imputation option 5:  Exclude the 2 variables with most misising values, 
# and drop any records with missing value in the remaining variables
dataset_Exclusive_Filtered <- select(dataset, -c(Insulin, SkinThickness))
dataset_Exclusive_Filtered <- dataset_Exclusive_Filtered[complete.cases(dataset_Exclusive_Filtered),]

# Imputation option 6:  Exclude the 2 variables with most misising values, 
# and use kNN to impute missing values in the remaining variables
dataset_Exclusive_kNN <- select(dataset_kNN, -c(Insulin, SkinThickness))
