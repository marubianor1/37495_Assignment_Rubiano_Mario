install.packages("lawstat")
library(lawstat)

install.packages("emmeans")
library(emmeans)

install.packages("ggplot2")
library(ggplot2)

weight <- c(179, 160, 136, 227, 217, 168, 108, 124, 183, 140,
            309, 229, 181, 141, 260, 203, 122, 169, 213, 257,
            243, 230, 248, 327, 329, 250, 180, 271, 316, 267,
            423, 340, 392, 339, 341, 226, 320, 295, 334, 322,
            325, 257, 303, 315, 380, 153, 263, 242, 206, 344,
            368, 390, 379, 260, 404, 318, 352, 359, 216, 222)

protein<- rep(c("horsebean", "linseed", "soybean", "sunflower", "meat", "casein" ), each=10)
data.Q1<-data.frame(weight,protein)
data.Q1$protein <- factor(data.Q1$protein,
                          levels = c("horsebean", "linseed", "soybean", "sunflower", "meat", "casein"))
boxplot(weight~protein, data=data.Q1)
fit.Q1<-aov(weight~protein, data=data.Q1)
summary(fit.Q1)

qqnorm(fit.Q1$residuals)
qqline(fit.Q1$residuals)
shapiro.test(fit.Q1$residuals)

#Shapiro-Wilk normality test

#data:  fit.Q1$residuals
#W = 0.97759, p-value = 0.3352 , higher than 0.05, residuals are normal distributed

plot(fit.Q1$residuals~fit.Q1$fitted.values) #No pattern, there is independence

leveneTest(data.Q1$weight, 
            data.Q1$protein,
            location = "median", 
            correction.method = "zero.correction")

#Levene's Test for Homogeneity of Variance (center = median: "median")
#      Df F value Pr(>F)
#group  5  0.8062 0.5503 #higher than 0.05, there is homogeneity of variance
#      54  

choose(6,2) #how many comparisons exist


meansObj<- emmeans(fit.Q1, "protein")
meansObj

#protein   emmean   SE df lower.CL upper.CL
#horsebean    164 17.9 54      128      200
#linseed      208 17.9 54      172      244
#soybean      266 17.9 54      230      302
#sunflower    333 17.9 54      297      369
#meat         279 17.9 54      243      315
#casein       327 17.9 54      291      363

fit.tukey <- contrast(object = meansObj, method = "pairwise", adjust = "tukey")
summary(fit.tukey, infer = c(T,T), level=0.95,side="two-sided")

#contrast              estimate   SE df lower.CL upper.CL t.ratio p.value
#horsebean - linseed      -44.2 25.4 54   -119.2    30.79  -1.741  0.5113 #No significant different
#horsebean - soybean     -101.9 25.4 54   -176.9   -26.91  -4.015  0.0024 #don't include 0, small p value -> highly significant
#horsebean - sunflower   -169.0 25.4 54   -244.0   -94.01  -6.659 <0.0001 
#horsebean - meat        -114.6 25.4 54   -189.6   -39.61  -4.515  0.0005
#horsebean - casein      -162.6 25.4 54   -237.6   -87.61  -6.406 <0.0001
#linseed - soybean        -57.7 25.4 54   -132.7    17.29  -2.273  0.2228
#linseed - sunflower     -124.8 25.4 54   -199.8   -49.81  -4.917  0.0001
#linseed - meat           -70.4 25.4 54   -145.4     4.59  -2.774  0.0774
#linseed - casein        -118.4 25.4 54   -193.4   -43.41  -4.665  0.0003
#soybean - sunflower      -67.1 25.4 54   -142.1     7.89  -2.644  0.1043
#soybean - meat           -12.7 25.4 54    -87.7    62.29  -0.500  0.9960
#soybean - casein         -60.7 25.4 54   -135.7    14.29  -2.392  0.1775
#sunflower - meat          54.4 25.4 54    -20.6   129.39   2.143  0.2813
#sunflower - casein         6.4 25.4 54    -68.6    81.39   0.252  0.9999
#meat - casein            -48.0 25.4 54   -123.0    26.99  -1.891  0.4186

plot(fit.tukey)


fit.dunnet <- contrast(object = meansObj, method = "trt.vs.ctrl", adjust = "mvt", ref=2)
summary(fit.dunnet)

#contrast              estimate   SE df t.ratio p.value
#linseed - horsebean       44.2 25.4 54   1.741  0.2920
#soybean - horsebean      101.9 25.4 54   4.015  0.0009
#sunflower - horsebean    169.0 25.4 54   6.659 <0.0001
#meat - horsebean         114.6 25.4 54   4.515  0.0001
#casein - horsebean       162.6 25.4 54   6.406 <0.0001

plot(fit.dunnet)

contrlist<-list("vege vs animal"=c(1,1,1,1,-2,-2))
fit.contrlist<- contrast(object=meansObj, method=contrlist, adjust="none")
summary(fit.contrlist)
plot(fit.contrlist)

#contrast       estimate   SE df t.ratio p.value
#vege vs animal     -239 62.2 54  -3.849  0.0003 #difference is -239, is high 


#Question 2

reaction_time <- c(0.168, 0.170, 0.181, 0.167, 0.182, 0.187, 0.202, 0.198, 0.236, # Auditory
                   0.257, 0.279, 0.269, 0.283, 0.235, 0.260, 0.240, 0.281, 0.258) # Visual

treatment <- factor(rep(c("Aud5", "Aud10", "Aud15", "Vis5", "Vis10", "Vis15"), each = 3),
                    levels = c("Aud5", "Aud10", "Aud15", "Vis5", "Vis10", "Vis15"))

data_exp <- data.frame(reaction_time, treatment)

boxplot(reaction_time ~ treatment, data = data_exp, 
        main = "Reaction Time by Treatment")

fit_exp <- aov(reaction_time ~ treatment, data = data_exp)
summary(fit_exp)

#Df   Sum Sq  Mean Sq F value   Pr(>F)    
#treatment    5 0.027834 0.005567    19.5 2.19e-05 ***
#  Residuals   12 0.003427 0.000286                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

shapiro.test(residuals(fit_exp))

#data:  residuals(fit_exp)
#W = 0.96314, p-value = 0.6632 Higher than 0.05, check

leveneTest(reaction_time ~ treatment, data = data_exp)
#      Df F value Pr(>F)
#group  5  0.4697 0.7919 Higher than 0.05, check
#      12 

contrasts_list <- list(
  "Aud vs Vis" = c(1, 1, 1, -1, -1, -1),
  "Lag 5 Aud vs Vis" = c(1, 0, 0, -1, 0, 0),
  "Lag 10 Aud vs Vis" = c(0, 1, 0, 0, -1, 0),
  "Lag 15 Aud vs Vis" = c(0, 0, 1, 0, 0, -1)
)

means_exp <- emmeans(fit_exp, "treatment")
final_contrasts <- contrast(means_exp, contrasts_list, adjust = "scheffe")
summary(final_contrasts, infer = c(T, T))
