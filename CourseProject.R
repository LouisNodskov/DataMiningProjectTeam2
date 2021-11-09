############################
# R Code for Final Project #
############################

# Install Packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("naivebayes")
install.packages("party")
install.packages("randomForest")
library(ggplot2)
library(dplyr)
library(naivebayes)
library(randomForest)
library(party)

# set your working directory to the current file directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read DataSet from CSV and add column headers
dataset <- read.csv("Data-Files/Census-Project.data", header = FALSE)
colnames(dataset) <- c("Age", "WorkClass", "fnlwgt", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain", "CapitalLoss", "HoursPerWeek", "NativeCountry", "Income")

# Replace ? NA with and remove lines missing values
dataset[dataset == "?"] <- NA
#dataset$Income <- replace(dataset$Income, dataset$Income=='<=50K', 0)
#dataset$Income <- replace(dataset$Income, dataset$Income=='>50K', 1)
dataset$Income <- as.factor(dataset$Income)
initialSet <- na.omit(dataset)

# Consolidating Data Attributes to be used in analysis
## Education
LessThanHS <- c("Preschool", "1st-4th", "5th-6th", "7th-8th")
initialSet$Education[initialSet$Education %in% LessThanHS] <- "LessThanHS"
someHS <- c("9th", "10th", "11th", "12th")
initialSet$Education[initialSet$Education %in% someHS] <- "Some-HS"
Assoc <- c("Assoc-acdm", "Assoc-voc")
initialSet$Education[initialSet$Education %in% Assoc] <- "Assoc"
initialSet$Education <- as.factor(initialSet$Education)
initialSet$Education <- factor(initialSet$Education, levels = c("LessThanHS", "Some-HS", "HS-grad", "Some-college", "Assoc", "Prof-school", "Bachelors", "Masters", "Doctorate"))
## MaritalStatus
Married <- c("Married-civ-spouse", "Married-AF-spouse", "Married-spouse-absent")
initialSet$MaritalStatus[initialSet$MaritalStatus %in% Married] <- "Married"
initialSet$MaritalStatus <- as.factor(initialSet$MaritalStatus)
## Work Class
selfempClass <- c("Self-emp-inc", "Self-emp-not-inc")
initialSet$WorkClass[initialSet$WorkClass %in% selfempClass] <- "Self-emp"
govClass <- c("State-gov", "Federal-gov", "Local-gov")
initialSet$WorkClass[initialSet$WorkClass %in% govClass] <- "Government"
initialSet$WorkClass <- as.factor(initialSet$WorkClass)
## Occupation
initialSet$Occupation <- as.factor(initialSet$Occupation)


# Summarize Data
table(initialSet$WorkClass)
table(initialSet$Occupation)
table(initialSet$Education)
table(initialSet$MaritalStatus)
table(initialSet$Race)
table(initialSet$Sex)
table(initialSet$NativeCountry)

# Plotting
ggplot(initialSet, aes(y=Age, x=Income, fill=Income)) + geom_boxplot() + ggtitle("Age Distribution in Each Income Class")
ggplot(initialSet, aes(x=Income, y=Age, fill=Income)) + geom_violin(trim = FALSE, adjust=2) + geom_boxplot(width=0.1, color="grey", fill="white")
ggplot(initialSet, aes(x=Race, fill=Race)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Races in Dataset")
ggplot(initialSet, aes(x=Sex, fill=Sex)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Sexes in Dataset")
ggplot(initialSet, aes(x=Education, fill=Education)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Education in Dataset")
ggplot(initialSet, aes(x=MaritalStatus, fill=MaritalStatus)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Marital Status in Dataset")
ggplot(initialSet, aes(x=WorkClass, fill=WorkClass)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Work Class in Dataset")
ggplot(initialSet, aes(x=Occupation, fill=Occupation)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Occupation in Dataset")
ggplot(initialSet, aes(x=WorkClass, fill=Occupation)) + geom_bar(position="stack", stat="count", color="black") + ggtitle("Distribution of Occupation Per Work Class")

ggplot(initialSet, aes(x=Income, fill=Race)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Races in Dataset based on Income Class")
ggplot(initialSet, aes(x=Race, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Races in Dataset based on Income Class")

ggplot(initialSet, aes(x=Income, fill=Sex)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Sexes in Dataset based on Income Class")
ggplot(initialSet, aes(x=Sex, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Sexes in Dataset based on Income Class")

ggplot(initialSet, aes(x=Income, fill=MaritalStatus)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Marital Status per Income Class")
ggplot(initialSet, aes(x=MaritalStatus, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Marital Status per Income Class")

ggplot(initialSet, aes(x=Income, fill=WorkClass)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Work-Classes per Income Class")
ggplot(initialSet, aes(x=WorkClass, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Work-Classes per Income Class")

ggplot(initialSet, aes(x=Income, fill=Occupation)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Occupations per Income Class")
ggplot(initialSet, aes(x=Occupation, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Occupations per Income Class")

ggplot(initialSet, aes(x=Income, fill=Education)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Education per Income Class")
ggplot(initialSet, aes(x=Education, fill=Income)) + geom_bar(position="dodge", stat="count") + ggtitle("Distribution of Education per Income Class")

#ggplot(initialSet, aes(x=Income, fill=Relationship)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Familial Relationship Status per Income Class")
# This one is kind of insane - we need a better way of showing: ggplot(initialSet, aes(x=Income, fill=NativeCountry)) + geom_bar(position="stack", stat="count") + ggtitle("Distribution of Country of Origin per Income Class")
# I tried this one as boxplot and scatter plot and it just looks jank - ggplot(initialSet, aes(x=Income, y=HoursPerWeek, fill=Income)) + geom_boxplot()

# Transform Categorical Data to Numeric Data
## Created a test Dataset to try this on 
analysisSet <- initialSet
### analysisSet$NativeCountry[analysisSet$NativeCountry !="United-States"] <- "Outside-US"
analysisSet$NativeCountry[analysisSet$NativeCountry !="United-States"] <- 1
analysisSet$NativeCountry[analysisSet$NativeCountry == "United-States"] <- 0

analysisSet$Sex[analysisSet$Sex == "Male"] <- 0.0
analysisSet$Sex[analysisSet$Sex == "Female"] <- 1.0
analysisSet$Sex <- as.integer(analysisSet$Sex)

analysisSet$Education <- sapply(analysisSet$Education, unclass)
maxEducation <- as.integer(max(analysisSet$Education))
analysisSet <- transform(analysisSet, Education = Education / maxEducation, 1)

maxAge <- max(analysisSet$Age)
analysisSet <- transform(analysisSet, Age = Age / maxAge)

analysisSet$MaritalStatus <- sapply(analysisSet$MaritalStatus, unclass)
maxMaritalStatus <- as.integer(max(analysisSet$MaritalStatus))
analysisSet <- transform(analysisSet, MaritalStatus = MaritalStatus/maxMaritalStatus, 1)

analysisSet$WorkClass <- sapply(analysisSet$WorkClass, unclass)
maxWorkClass <- as.integer(max(analysisSet$WorkClass))
analysisSet <- transform(analysisSet, WorkClass = WorkClass/maxWorkClass, 1)

analysisSet$Occupation <- sapply(analysisSet$Occupation, unclass)
maxOccupation <- as.integer(max(analysisSet$Occupation))
analysisSet <- transform(analysisSet, Occupation = Occupation/maxOccupation, 1)

# Decision Tree

index <- sample(2, nrow(analysisSet), replace=TRUE, prob=c(0.8, 0.2))
TrainingSet <- analysisSet[index==1,]
TestSet <- analysisSet[index==2,]
Formula <- Income ~ Age + WorkClass + Education + MaritalStatus + Occupation + Sex
cTree <- ctree(Formula, data=TrainingSet)
train_predict <- predict(cTree, TrainingSet)
table(train_predict, TrainingSet$Income)
test_predict <- predict(cTree, TestSet)
table(test_predict, TestSet$Income)

# Random Forest

rf <- randomForest(formula=Formula, data=TrainingSet)
plot(rf)

train_predict <- predict(rf, TrainingSet)
t1 <- table(train_predict, TrainingSet$Income)
1 - sum(diag(t1)) / sum(t1)
test_predict <- predict(rf, TestSet)
t2 <- table(test_predict, TestSet$Income)
1 - sum(diag(t2)) / sum(t2)

# Naive Bayes

nb <- naive_bayes(formula=Formula, data=TrainingSet)
plot(nb)

train_predict <- predict(nb, TrainingSet)
t1 <- table(train_predict, TrainingSet$Income)
1 - sum(diag(t1)) / sum(t1)
test_predict <- predict(nb, TestSet)
t2 <- table(test_predict, TestSet$Income)
1 - sum(diag(t2)) / sum(t2)


