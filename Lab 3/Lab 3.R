install.packages(c("nortest", "lawstat", "DescTools", "car", "MASS"))
library(nortest)
library(lawstat)
library(DescTools)
library(car)
library(MASS)

#Question 1
#a)
#The Linear ModelFor a one-way ANOVA, the linear statistical model is:
#y_{ij} = mu + tau_i + epsilon_{ij}


#Components:
#y_{ij}$: The j-th observation (tensile strength) from the i-th treatment level (cotton percentage).
#mu: The overall mean tensile strength across all groups.
#tau_i: The effect of the i-th level of cotton percentage (where i = 15, 20, 25, 30, 35).
#epsilon_{ij}: The random error component associated with the j-th observation in the i-th group.

#b)

#c) 
strength<- c(7,7,15,11,9,12,17,12,9,18,14,10,19,18,18,19,25,22,19,23,7,10,19,15,11)
cotton<- rep(c("15%","20%","25%","30%","35%"), each=5)
dataQ1<-data.frame(strength,cotton)

fitQ1<-aov(strength~cotton, data=dataQ1)
summary(fitQ1)

residQ1<-fitQ1$residuals

qqnorm(residQ1)
qqline(residQ1)

kruskal.test(strength~cotton, data=dataQ1)





#Question 2

hours<-c(1953,2135,2471,4727,6134,5314,
         1190, 1286, 1550, 2125, 2557, 2845,
         651, 837, 848, 1038, 1141, 1543,
         511, 651, 651, 652, 688, 480)
temp<-rep(c(825,885,905,930), each=6)
dataQ2<-data.frame(hours,temperature=as.factor(temp))

fitQ2<-aov(hours~temperature, data=dataQ2)
summary(fitQ2)

residQ2<-fitQ2$residuals

qqnorm(residQ2)
qqline(residQ2)

shapiro.test(residQ2)
nortest::cvm.test(residQ2)
nortest::ad.test(residQ2)
plot(residQ2~fitQ2$fitted.values)

LeveneTest(fitQ2)
durbinWatsonTest(fitQ2)

dataQ2$hour.inv <- 1/hours

fitQ2b <- aov(hour.inv ~ temperature, data = dataQ2)
summary(fitQ2b)

residQ2b<-fitQ2b$residuals

qqnorm(residQ2b)
qqline(residQ2b)

shapiro.test(residQ2b)
nortest::cvm.test(residQ2b)
nortest::ad.test(residQ2b)
plot(residQ2b~fitQ2b$fitted.values)

LeveneTest(fitQ2b)
durbinWatsonTest(fitQ2b)

boxcox(fitQ2,lambda=seq(-3,3,length=100))
parlist <- boxcox(fitQ2,lambda=seq(-3,3,length=100),plotit = F)
lambda_max <- parlist$x[which.max(parlist$y)]

bc <- boxcox(hours ~ temperature, data = dataQ2)