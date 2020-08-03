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

#The probability of temperature going outside 2-4 C is given #by 
#P( x < 2 | mean = 3.002 , sd = 0.466) + P( x > 4 | mean = ##3.002 , sd = 0.466)  	
#(1 - pnorm(2, mean = 3.002 , sd = 0.466 , lower.tail = #FALSE)) + pnorm(4, mean = 3.002, sd = 0.466, lower.tail = #FALSE)	
#Probability of Temperature going outside 2-4 C is 0.032 #which is above 2.5% and below 5%. So, the Penalty will be ##10% of the AMC (Annual Maintenance Contract).


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
#Null Hypothesis: Sample means of Cold storage temperature #between Rainy, summer and Winter Seasons are same.
#H0: Mean (Summer) = Mean (Winter) = Mean (Rainy)
#Alternate Hypothesis: There is statistical significant #difference of Sample mean in at least one of Seasons: #Rainy, Summer & Winter.
#HA: H0 is not true
#F Value is 25.32 and P value is 5.08e -11
#At 0.01 and 0.05 level, the p value is very small and we do #reject the null hypothesis and say that there is #statistically significant difference in sample mean #temperature between the seasons and at least one of the #seasons is different from other seasons.
#As there are three seasons, and to have an additional #insight of the season that is statistically significant #difference in sample mean temperature, we perform Tukey #test with 95% confidence level which helps us to compare #the seasons in pairs.

TukeyHSD(ColdStorageModel)
#P Value between summer and rainy season is 0.5 which #is #greater than 0.05, so we fail to reject null hypothesis #and #we cannot conclude that the sample mean #temperature was #different for summer and rainy #season.
#But, P value between winter season and #Summer/Rainy Season #is 0 and we reject the null #hypothesis and say that winter #season has #statistically significant mean temperature from #the other two seasons.

#======================================================


#Sample Size is 35 and population deviation is not known. #Generally z-tests are used when we have large sample sizes #(n > 30), whereas t-tests are used with a smaller sample #size (n < 30). Also, z-tests are performed when the #population standard deviation is known and t-tests are done #when the population standard deviations is not known. 

#As per the given case study, the sample size is 35 which is #considered to be a larger sample size as it’s greater than #30 which is considered to value that define the #larger/smaller sample size. But, the population standard #deviation is unknown. So, t-test will be performed to check #if the corrective action is needed at the cold storage #plant.

#======================================================
#Level of significance (Alpha) = 0.1
#The sample size, N=35 and population standard deviation is #unknown so we will use T-Test.
#Degree of Freedom: we have N-1 degrees of freedom: 34
#The sole purpose of the test is check whether temperature #maintained is greater than 3.9 C so we are interested in one #direction. Therefore, it is one Tail T test.
#HYPOTHESIS FORMULATION:
#Null Hypothesis: Sample mean temperature is maintained less #than 4C
#H0: Mean(Temperature) is less than or equal to 3.9 #Alternate Hypothesis: Sample mean temperature is above 3.9C
#HA: Mean(Temperature) > 3.9 C

#======================================================
TemperatureMean = mean(ColdStorageProb2$Temperature)
TemperatureMean
TemperatureSD = sd(ColdStorageProb2$Temperature)
TemperatureSD
hist(ColdStorageProb2$Temperature)
attach(ColdStorageProb2)
t.test(ColdStorageProb2$Temperature,mu = 3.9,alternative = 'greater')

#======================================================
# p value = 0.0047 < 0.1  - reject null hypothesis
# we can say that result is statistically significant and # #the mean temperature is above 3.9C. So, there is a need for #corrective action to be taken by cold storage plant. #Additional and vigilant measures can be put up for strictly #maintaining temperature less than 3.9C and then after some #days, data can be gathered again and analyzed. Further #research can be done to check if the temperature greater #than 3.9C is observed for consecutive days and if so, the #temperature in procurement side can be gathered and #analyzed to confirm if the problem is aroused due to #procurement. 



