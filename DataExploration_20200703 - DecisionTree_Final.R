#Data Exploration Course 
#Merrimack College - Summer 2020
#Decision Tree

#Invoke the requried library packages
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(rpart) #Decision Tree
library(rattle) #Decision Tree
library(rpart.plot) #Decision Tree
library(RColorBrewer) #Decision Tree

#set the working directory to start with in your local machine
setwd ("D:/Merrimack/DSA5100A - Data Exploration/Week 4/Segmentation Project")

#Read in the customer data
customer_data <- fread("D:/Merrimack/DSA5100A - Data Exploration/Week 4/Segmentation Project/Customer_Dataset_File.csv", stringsAsFactors=FALSE)

#These fields have a $ sign and a comma and therefore needs to be converted to a numeric vector
#Overwrite existing variables with cleansed version
customer_data$Age = as.numeric(customer_data$Age)
customer_data$EducationYears = as.numeric(customer_data$EducationYears)
customer_data$CardTenure = as.numeric(customer_data$CardTenure)
customer_data$HHIncome = as.numeric(gsub('[$,]', '', customer_data$HHIncome))
customer_data$CardSpendMonth = as.numeric(gsub('[$,]', '', customer_data$CardSpendMonth))
customer_data$VoiceLastMonth = as.numeric(gsub('[$,]', '', customer_data$VoiceLastMonth))
customer_data$VoiceOverTenure = as.numeric(gsub('[$,]', '', customer_data$VoiceOverTenure))
customer_data$EquipmentLastMonth = as.numeric(gsub('[$,]', '', customer_data$EquipmentLastMonth))
customer_data$EquipmentOverTenure = as.numeric(gsub('[$,]', '', customer_data$EquipmentOverTenure))
customer_data$DataLastMonth = as.numeric(gsub('[$,]', '', customer_data$DataLastMonth))
customer_data$DataOverTenure = as.numeric(gsub('[$,]', '', customer_data$DataOverTenure))

#Replacing missing values with means
customer_data$HouseholdSize[is.na(customer_data$HouseholdSize)] = mean(customer_data$HouseholdSize, na.rm=TRUE)
customer_data$NumberPets[is.na(customer_data$NumberPets)] = mean(customer_data$NumberPets, na.rm=TRUE)
customer_data$CommuteTime[is.na(customer_data$CommuteTime)] = mean(customer_data$CommuteTime, na.rm=TRUE)
customer_data$CardSpendMonth[is.na(customer_data$CardSpendMonth)] = mean(customer_data$CardSpendMonth, na.rm=TRUE)
customer_data$PhoneCoTenure[is.na(customer_data$PhoneCoTenure)] = mean(customer_data$PhoneCoTenure, na.rm=TRUE)
customer_data$VoiceOverTenure[is.na(customer_data$VoiceOverTenure)] = as.numeric(0, na.rm=TRUE)
customer_data$EquipmentLastMonth[is.na(customer_data$EquipmentLastMonth)] = as.numeric(0, na.rm=TRUE)
customer_data$EquipmentOverTenure[is.na(customer_data$EquipmentOverTenure)] = as.numeric(0, na.rm=TRUE)
customer_data$DataLastMonth[is.na(customer_data$DataLastMonth)] = as.numeric(0, na.rm=TRUE)
customer_data$DataOverTenure[is.na(customer_data$DataOverTenure)] = as.numeric(0, na.rm=TRUE)

#Convert all missing values for gender to female - arbitrary decision
customer_data$Gender[is.na(customer_data$Gender)]  <-  "Female"

#Convert all missing values for TownSize to 3 - arbitrary decision
customer_data$TownSize[is.na(customer_data$TownSize)]  <-  3

#Convert all missing values for HomeOwner to 0 (No) - arbitrary decision
customer_data$HomeOwner[is.na(customer_data$HomeOwner)]  <-  0

#Convert Yes/No responses to 1s and 2s
#sum(is.na(customer_data$ActiveLifestyle))
customer_data$Gender <- ifelse(customer_data$Gender == "Female", 1, 0)
customer_data$Retired <- ifelse(customer_data$Retired == "Yes", 1, 0)
customer_data$MaritalStatus <- ifelse(customer_data$MaritalStatus == "Unmarried", 1, 0)
customer_data$ActiveLifestyle <- ifelse(customer_data$ActiveLifestyle == "Yes", 1, 0)
customer_data$EBilling <- ifelse(customer_data$EBilling == "Yes", 1, 0)
customer_data$Internet <- ifelse(customer_data$Internet == "Yes", 1, 0)
customer_data$Pager <- ifelse(customer_data$Pager == "Yes", 1, 0)
customer_data$OwnsPC <- ifelse(customer_data$OwnsPC == "Yes", 1, 0)
customer_data$OwnsMobileDevice <- ifelse(customer_data$OwnsMobileDevice == "Yes", 1, 0)
customer_data$OwnsGameSystem <- ifelse(customer_data$OwnsGameSystem == "Yes", 1, 0)
customer_data$NewsSubscriber <- ifelse(customer_data$NewsSubscriber == "Yes", 1, 0)


#If you want to get the outliers but don't need to see the boxplot set plot=FALSE
outliers <- boxplot(customer_data$HHIncome, plot=FALSE)$out
#Create a duplicate cust_data dataframe to hold our HHIncome object
cust_data1 <- customer_data

#So here the "-which" is looking for rows in cust_data1 that are NOT in the outliers object and
#putting them in the new dataframe, NoHHIouts
#This command creates a new dataframe that does not have any outliers for HHIncome
customer_data <- cust_data1[-which(cust_data1$HHIncome %in% outliers),]
rm(cust_data1)

#Fix 0s in PhoneCoTenure field - only 3 or 4 rows
#Take last month voice for voice over tenure = 0 and set phonecotenure = 1
select <- customer_data$PhoneCoTenure == 0
customer_data[select,"VoiceOverTenure"] <- customer_data[select,"VoiceLastMonth"]
customer_data$PhoneCoTenure <- ifelse(customer_data$PhoneCoTenure == 0, 1, customer_data$PhoneCoTenure)

#Recode Age into a 6 level variable
customer_data <- mutate(customer_data, Age_Buckets = case_when(
  Age %in%  18:24   ~ 1,
  Age %in%  25:34   ~ 2,
  Age %in%  35:44   ~ 3,
  Age %in%  45:54   ~ 4,
  Age %in%  55:64   ~ 5,
  Age >=     65      ~ 6
))

#Create Variables
customer_data$TechAdopter <- ifelse(customer_data$Internet == 1 & customer_data$EBilling == 1 & customer_data$OwnsMobileDevice == 1, 1, 0)
customer_data$TotalOverTenure = as.numeric(customer_data$DataOverTenure+customer_data$EquipmentOverTenure+customer_data$VoiceOverTenure)
customer_data$MonthlyOverTenure = as.numeric((customer_data$DataOverTenure+customer_data$EquipmentOverTenure+customer_data$VoiceOverTenure)/customer_data$PhoneCoTenure)
customer_data$VoiceOnly <- ifelse(customer_data$EquipmentRental == "No" & customer_data$WirelessData == "No",1,0)

#Recode PhoneCoTenure into a 6 level variable
customer_data <- mutate(customer_data, PCoTenure_HighLow = case_when(
  PhoneCoTenure %in%  0:36   ~ 0,
  PhoneCoTenure >=  37   ~ 1,
))

#Recode MonthlyOverTenure into a 2 level variable
customer_data <- mutate(customer_data, MonthlyOT_HighLow = case_when(
  customer_data$MonthlyOverTenure <= 48   ~ 0,
  customer_data$MonthlyOverTenure >  48   ~ 1,
))

#Average variables
customer_data$AvgVoiceOverTenure = as.numeric(customer_data$VoiceOverTenure/customer_data$PhoneCoTenure)
customer_data$AvgEquipOverTenure = as.numeric(customer_data$EquipmentOverTenure/customer_data$PhoneCoTenure)
customer_data$AvgWDOverTenure = as.numeric(customer_data$DataOverTenure/customer_data$PhoneCoTenure)
customer_data$AvgTotalOverTenure = (customer_data$DataOverTenure+customer_data$EquipmentOverTenure+customer_data$VoiceOverTenure)/customer_data$PhoneCoTenure


##---Decision Tree Analysis---##

## KEEPERS
#--DT1
fit <- rpart(MonthlyOT_HighLow ~ TechAdopter + Age_Buckets,
             data=customer_data,
             method="class",
             control=rpart.control(minsplit=2, cp=0)
)
fancyRpartPlot(fit)

#--DT2
fit <- rpart(MonthlyOT_HighLow ~ Age + EBilling + OwnsPC + OwnsMobileDevice,
             data=customer_data,
             method="class"
             #,control=rpart.control(minsplit=3, cp=0)
)
fancyRpartPlot(fit)

#--DT3
fit <- rpart(MonthlyOT_HighLow ~ AvgVoiceOverTenure + AvgWDOverTenure,
             data=customer_data,
             method="class",
             #control=rpart.control(minsplit=1, cp=0)
)
fancyRpartPlot(fit)


#--DT4
fit <- rpart(MonthlyOT_HighLow ~ TechAdopter + Age_Buckets + VoiceOnly,
             data=customer_data,
             method="class",
             control=rpart.control(minsplit=2, cp=0)
)
fancyRpartPlot(fit)

#--DT5
fit <- rpart(MonthlyOT_HighLow ~ AvgVoiceOverTenure + AvgWDOverTenure + AvgEquipOverTenure,
             data=customer_data,
             method="class",
             #control=rpart.control(minsplit=1, cp=0)
)
fancyRpartPlot(fit)

                               
     