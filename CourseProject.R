############################
# R Code for Final Project #
############################

# Install Packages
install.packages("ggplot2")
library(ggplot2)

# set your working directory to the current file directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read DataSet from CSV and add column headers
dataset <- read.csv("Data-Files/Census-Project.data", header = FALSE)
colnames(dataset) <- c("Age", "WorkClass", "fnlwgt", "Education", "EducationNum", "MaritalStatus", "Occupation", "Relationship", "Race", "Sex", "CapitalGain", "CapitalLoss", "HoursPerWeek", "NativeCountry", "Income")

# Replace ? with and remove lines missing values
dataset[dataset == "?"] <- NA
noNaValues <- na.omit(dataset)

# Summarize Data


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
# Normalize Data #
##################



dataset$Income <- replace(dataset$Income, dataset$Income=='<=50K', 0)
dataset$Income <- replace(dataset$Income, dataset$Income=='>50K', 1)
