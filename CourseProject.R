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

# Plotting
ggplot(noNaValues, aes(y=Age, x=Income)) + geom_boxplot() + ggtitle("Average Age of Each Income Class")

# Normalize Data #
##################



dataset$Income <- replace(dataset$Income, dataset$Income=='<=50K', 0)
dataset$Income <- replace(dataset$Income, dataset$Income=='>50K', 1)
