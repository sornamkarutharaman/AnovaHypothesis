#======================================================
#     Cold Store - Anova & Hypothesis Testing
#======================================================

#Setting working directory
setwd("<Project Directory>")

#Verify the Working Directory
getwd()

#Install necessary Packages 
install.packages("pacman")
library(pacman)
p_load(readr,readxl,dplyr,ggplot2)


#Loading Dataset & View 
#Cold_Storage_Temp_Data read_csv("<file>")
View(Cold_Storage_Temp_Data)

# DataSummary 
summary(Cold_Storage_Temp_Data)

#Change Datatype
Cold_Storage_Temp_Data$Season = as.factor(Cold_Storage_Temp_Data$Season)
Cold_Storage_Temp_Data$Month = as.factor(Cold_Storage_Temp_Data$Month)

#Access variable names
attach(Cold_Storage_Temp_Data)

#Histogram and box plots
boxplot(Temperature~Season,horizontal=TRUE,col=c("Blue","Green","Pink"))

ggplot(Cold_Storage_Temp_Data , aes(Temperature, fill = Season))+ geom_bar()

ggplot(Cold_Storage_Temp_Data, aes(x = Season, y = Temperature , fill = Season)) + geom_bar(stat = "identity") + theme_classic()

#======================================================
#mean temperature for Summer, Winter and Rainy Season
#======================================================

by(Cold_Storage_Temp_Data$Temperature,INDICES = Cold_Storage_Temp_Data$Season,FUN=mean)

#======================================================
#overall mean for the full year 
#======================================================

OverallMean = mean(Cold_Storage_Temp_Data$Temperature)
OverallMean 

#======================================================
#Standard Deviation for the full year
#======================================================
SDTemperature = sd(Cold_Storage_Temp_Data$Temperature)
SDTemperature

#======================================================
#Assuming Normal distribution,probability of temperature #having fallen below 2 C
#======================================================
p(x <2 | mean = 3.002 and SD = 0.466 )
1 - pnorm(2, mean = 3.002 , sd = 0.466 , lower.tail = FALSE)

#======================================================
#Assume Normal distribution,
#Probability of temperature having gone above 4 C?
#======================================================
p( x > 4 | mean = 3.002 , sd = 0.466)
pnorm(4, mean = 3.002, sd = 0.466, lower.tail = FALSE)


#======================================================
# Penalty for the AMC Company
#======================================================

 p( x < 2 | mean = 3.002 , sd = 0.466) + p( x > 4 | mean = 3.002 , sd = 0.466)
(1 - pnorm(2, mean = 3.002 , sd = 0.466 , lower.tail = FALSE)) + pnorm(4, mean = 3.002, sd = 0.466, lower.tail = FALSE)

#======================================================
# one-way ANOVA test to determine if there is a
# significant difference in Cold Storage temperature between 
# rainy, summer and winter seasons and comment on the findings.
#======================================================
attach(Cold_Storage_Temp_Data)
require(ggplot2)

ggplot(Cold_Storage_Temp_Data, aes(x = Season, y = Temperature)) + 
  geom_boxplot(fill = "grey80", colour = "blue") + 
  scale_x_discrete() + 
  xlab("Season") + 
  ylab("Temperature")  
 
ColdStorageModel=aov(Temperature~Season,data=Cold_Storage_Temp_Data)

summary(ColdStorageModel)
#======================================================
# P value is very small- we reject null hypothesis
# Means are different
TukeyHSD(ColdStorageModel)
#Mean of Winter is significantly different from Means of #summer and rainy.
#P is low and less than 0.05 - so we reject null hypothesis. 
#Summer and Rainy --> p value is greater than 0.05 and we #fail to reject 
#======================================================



#======================================================
#level of significance (Alpha) = 0.1
#sample size = 35 and standard devoation is unknown - T stat
#DF = N-1 = 34
# COld storage getting complaints - sour/smell
# wants to maintain <= 3.9C
# pulls out last 35 days temperature. Significance level - 0.1
# H0 <= 3.9 , HA >3.9
#======================================================
TemperatureMean = mean(ColdStorageProb2$Temperature)
TemperatureMean
TemperatureSD = sd(ColdStorageProb2$Temperature)
TemperatureSD
hist(ColdStorageProb2$Temperature)
attach(ColdStorageProb2)
t.test(ColdStorageProb2$Temperature,mu = 3.9,alternative = 'greater')
# p value = 0.04 < 0.1  - reject null hypothesus
# its  greater than 3.9 



