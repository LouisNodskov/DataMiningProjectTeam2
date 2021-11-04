############################
# R Code for Final Project #
############################

# Install Packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)

# set your working directory to the current file directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read DataSet from CSV and add column headers
dataset <- read.csv("Data-Files/Census-Project.data", header = FALSE)
colnames(dataset) <- c("Age", "WorkClass", "fnlwgt", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain", "CapitalLoss", "HoursPerWeek", "NativeCountry", "Income")

# Replace ? with and remove lines missing values
dataset[dataset == "?"] <- NA
#dataset$Income <- replace(dataset$Income, dataset$Income=='<=50K', 0)
#dataset$Income <- replace(dataset$Income, dataset$Income=='>50K', 1)
dataset$Income <- as.factor(dataset$Income)
noNaValues <- na.omit(dataset)

# Summarize Data
table(noNaValues$WorkClass)
table(noNaValues$Occupation)
table(noNaValues$Education)
table(noNaValues$MaritalStatus)
table(noNaValues$Race)
table(noNaValues$Sex)
table(noNaValues$NativeCountry)

# Plotting
ggplot(noNaValues, aes(y=Age, x=Income, fill=Income)) + geom_boxplot() + ggtitle("Average Age of Each Income Class")
ggplot(noNaValues, aes(y=Race, fill=Race)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Races in Dataset")
ggplot(noNaValues, aes(x=Income, fill=Race)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Races in Dataset based on Income Class")
ggplot(noNaValues, aes(x=Income, fill=MaritalStatus)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Marital Status per Income Class")
ggplot(noNaValues, aes(x=Income, fill=WorkClass)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Work-Classes per Income Class")
ggplot(noNaValues, aes(x=Income, fill=Occupation)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Occupations per Income Class")
ggplot(noNaValues, aes(x=Income, fill=Relationship)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Familial Relationship Status per Income Class")
# This one is kind of insane - we need a better way of showing: ggplot(noNaValues, aes(x=Income, fill=NativeCountry)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Country of Origin per Income Class")
# I tried this one as boxplot and scatter plot and it just looks jank - ggplot(noNaValues, aes(x=Income, y=HoursPerWeek, fill=Income)) + geom_boxplot()

# Normalize Data
## Created a test Dataset to try this on 
testSet <- noNaValues
### testSet$NativeCountry[testSet$NativeCountry !="United-States"] <- "Outside-US"
testSet$NativeCountry[testSet$NativeCountry !="United-States"] <- 1
testSet$NativeCountry[testSet$NativeCountry == "United-States"] <- 0

testSet$Sex[testSet$Sex == "Male"] <- 0.0
testSet$Sex[testSet$Sex == "Female"] <- 1.0
testSet$Sex <- as.integer(testSet$Sex)

LessThanHS <- c("Preschool", "1st-4th", "5th-6th", "7th-8th")
testSet$Education[testSet$Education %in% LessThanHS] <- "LessThanHS"
someHS <- c("9th", "10th", "11th", "12th")
testSet$Education[testSet$Education %in% someHS] <- "Some-HS"
Assoc <- c("Assoc-acdm", "Assoc-voc")
testSet$Education[testSet$Education %in% Assoc] <- "Assoc"
testSet$Education <- as.factor(testSet$Education)
testSet$Education <- factor(testSet$Education, levels = c("LessThanHS", "Some-HS", "HS-grad", "Some-college", "Assoc", "Prof-school", "Bachelors", "Masters", "Doctorate"))
testSet$Education <- sapply(testSet$Education, unclass)
maxEducation <- as.integer(max(testSet$Education))
testSet <- transform(testSet, Education = Education / maxEducation, 1)

maxAge <- max(testSet$Age)
testSet <- transform(testSet, Age = Age / maxAge)

Married <- c("Married-civ-spouse", "Married-AF-spouse", "Married-spouse-absent")
testSet$MaritalStatus[testSet$MaritalStatus %in% Married] <- "Married"
testSet$MaritalStatus <- as.factor(testSet$MaritalStatus)
testSet$MaritalStatus <- sapply(testSet$MaritalStatus, unclass)
maxMaritalStatus <- as.integer(max(testSet$MaritalStatus))
testSet <- transform(testSet, MaritalStatus = MaritalStatus/maxMaritalStatus, 1)

selfempClass <- c("Self-emp-inc", "Self-emp-not-inc")
testSet$WorkClass[testSet$WorkClass %in% selfempClass] <- "Self-emp"
govClass <- c("State-gov", "Federal-gov", "Local-gov")
testSet$WorkClass[testSet$WorkClass %in% govClass] <- "Government"
testSet$WorkClass <- as.factor(testSet$WorkClass)
testSet$WorkClass <- sapply(testSet$WorkClass, unclass)
maxWorkClass <- as.integer(max(testSet$WorkClass))
testSet <- transform(testSet, WorkClass = WorkClass/maxWorkClass, 1)

testSet$Occupation <- as.factor(testSet$Occupation)
testSet$Occupation <- sapply(testSet$Occupation, unclass)
maxOccupation <- as.integer(max(testSet$Occupation))
testSet <- transform(testSet, Occupation = Occupation/maxOccupation, 1)

index <- sample(2, nrow(testSet), replace=TRUE, prob=c(0.8, 0.2))
T1Set <- testSet[index==1,]
T2Set <- testSet[index==2,]
Formula <- Income ~ Age + WorkClass + Education + MaritalStatus + Occupation + Sex
cTree <- ctree(Formula, data=T1Set)
train_predict <- predict(cTree, T1Set)
table(train_predict, T1Set$Income
test_predict <- predict(cTree, T2Set)
table(test_predict, T2Set$Income)