#Question 1

#o compare tomato growth under different conditions, a Randomized Complete Block Design (RCBD) 
#is recommended to account for environmental gradients, such as light or temperature variations 
#in a greenhouse. In this design, the treatments are the specific growing conditions being 
#tested (e.g., three different nitrogen fertilizer levels). 

#The experimental units are the individual pots or plots containing a single tomato plant, 
#which are randomly assigned a treatment within each block. The measurement units are the 
#specific components of the plant being evaluated, such as the individual fruits or specific leaves. 

#Finally, an appropriate response variable would be the total dry shoot biomass (grams) 
#or the total fruit yield (kg) per plant, as these provide a quantifiable measure of 
#overall growth and productivity.

#Question 2

setwd("~/Library/CloudStorage/OneDrive-Personal/Documents/UTS/Semester 4/Statistical Design/Lab 2/")
getwd()

dataQ2 <- read.table("dataQ2.txt", header=TRUE, colClasses=(c("factor","numeric")))

#a)
stripchart(heads~treatment, vertical=TRUE, pch=1, data=dataQ2)

#b)
#The linear model for the Completely Randomized Design (CRD) in this study is:
#y_{ij} = \mu + \tau_i + \epsilon_{ij}where i = 1 to 5 (nitrogen rates) and j = 1 to 4 (replicate plots). 
#In this model, y_{ij} represents the observed number of lettuce heads for the j-th plot 
#receiving the i-th treatment. mu is the overall mean number of heads across all plots, 
#tau_i is the specific effect of the i-th nitrogen fertilizer rate (the deviation 
#from the mean caused by the treatment), and epsilon_{ij} is the random error term, 
#which represents the unexplained variation between individual plots. We assume the errors 
#are independent and normally distributed with a mean of zero and constant variance.

#c)
fit<-aov(heads~treatment, data=dataQ2)
summary(fit)
#Df Sum Sq Mean Sq F value Pr(>F)  
#treatment    4   5397  1349.2   3.774 0.0257 *
#  Residuals   15   5363   357.5 
#d) All means are not significantly different

#e)
#At least 1 is significantly different 

#f)
mu_hat<- mean(dataQ2$heads)
mu_hat

mu_hat_i <- tapply(dataQ2$heads, dataQ2$treatment, mean)
mu_hat_i

tau_hat_i <- mu_hat_i-mu_hat
tau_hat_i

#Question 3

dataQ3 <- read.csv("dataQ3.csv", header=TRUE, colClasses=(c("factor","numeric")))

dataQ3 <- dataQ3[!is.na(dataQ3$area),]
stripchart(area~cultivator, vertical=TRUE, pch=1, data=dataQ3)

boxplot(area~cultivator, data=dataQ3)

fit<-aov(area~cultivator, data=dataQ3)
summary(fit)

#Df Sum Sq Mean Sq F value  Pr(>F)    
#cultivator   3  304.7   101.6   14.11 1.4e-05 ***
#  Residuals   25  180.0     7.2                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#AT least 1 mean is significantly different

mu3_hat<- mean(dataQ3$area)
mu3_hat

mu3_hat_i <- tapply(dataQ3$area, dataQ3$cultivator, mean)
mu3_hat_i

tau3_hat_i <- mu3_hat_i-mu3_hat
tau3_hat_i

#Question 4
dataQ4 <- readRDS(file="dataQ4.RDS")

#a)

boxplot(time~diet, data=dataQ4)

#Differences in medians of coagulation times are evident across four diets, with Diet C holding 
#the highest (slowest) coagulation times, contrasting with lower (faster) times observed in 
#Diets D and A. 
#Variability, measured by the interquartile range, is greater in Diet D than in Diet C, 
#indicating potentially inconsistent responses among individual animals to Diet D. 
#Diet D may contain an outlier at a high value of 72, which diverges significantly 
#from other results. 

#The initial hypothesis posits that the lack of significant overlap between the interquartile 
#ranges of Diets C and A implies that diet type probably influences coagulation time, 
#a theory to be examined in the subsequent ANOVA analysis.

#b)

#The linear model for the Completely Randomized Design (CRD) used in Question 4 is:
#y_{ij} = mu + tau_i + epsilon_{ij}
#Indices:i = 1, 2, 3, 4 (representing the four diets: A, B, C, and D).
#j = 1, 2, 3, 4, 5, 6 (representing the six animals/replicates within each diet group).

#Model Components:
#y_{ij}: The observed coagulation time for the j-th animal assigned to the i-th diet.
#mu: The overall mean coagulation time across all diets and animals in the study.
#tau_i: The treatment effect of the i-th diet. It represents the deviation from the overall 
#mean specifically caused by that diet type.
#epsilon_{ij}: The random error (or residual) for the $j$-th animal in the $i$-th diet. T
#his accounts for the natural biological variation between animals and measurement errors. 
#We assume these errors are independent and identically distributed, following a normal 
#distribution: epsilon_{ij} sim N(0, sigma^2).

#c)
fit<-aov(time~diet, data=dataQ4)
summary(fit)

#Df Sum Sq Mean Sq F value Pr(>F)  
#diet         3  161.7   53.89   4.027 0.0216 *
#  Residuals   20  267.7   13.38                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#d)
#The null hypothesis assumes that the treatment has no effect. 
#In mathematical terms, it states that the mean response is the same for all groups:
#H_0: mu_1 = mu_2 = mu_3 = mu_4, in terms of the model components we defined earlier:
#H_0: tau_1 = tau_2 ... = \tau_k = 0(Translation: The specific effect of every treatment is zero).

#The Alternative Hypothesis ($H_1$ or $H_a$)
#The alternative hypothesis states that at least one treatment mean is different from the others:
#H_1: At least one  mu_i is different (or at least one } tau_i neq 0)

#e) From the ANOVA table, we obtain a p-value of approximately 0.0216. 
#Since this value is less than the standard significance level of alpha = 0.05, 
#we reject the null hypothesis.