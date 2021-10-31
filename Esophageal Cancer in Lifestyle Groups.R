rm(list = ls())
library(lsmeans) # for lsmeans function
library(car) # for Levene's Test
library(pwr)
esoph <- esoph
summary(esoph)

#box plots
par(mfrow = c(1, 3))
# box plot: Alcohol intake vs. Number of Cancer Cases
 
plot(esoph$ncases ~ esoph$alcgp, 
     xlab = "Daily Alcohol Intake (g)", 
     ylab = "Number of Cancer Cases", 
     main = "Alcohol Use vs. Cases", xaxt="n")
axis(1,at=1:4,labels=c("0-39",  "40-79", "80-119", "120+"))

# box plot: Tobacco intake vs. Number of Cancer Cases

plot(esoph$ncases ~ esoph$tobgp, 
     xlab = "Daily Tobacco Intake (g)", 
     ylab = "Number of Cancer Cases", 
     main = "Tobacco Use vs. Cases", xaxt="n")
axis(1,at=1:4,labels=c("0-9",  "10-19", "20-99", "30+"))

plot(esoph$ncases ~ esoph$agegp, 
     xlab = "Age Group", 
     ylab = "Number of Cancer Cases", 
     main = "Age Group vs. Cases", xaxt="n")
axis(1,at=1:6,labels=c("25-34",  "35-44", "45-54", "55-64", "65-74", "75+"))



######### Plots for three-way interaction ######### 
par(mfrow = c(3, 2))
# plots for three-way interaction, agegp as the third factor
# 25-34
esoph1 <- esoph[esoph$agegp == "25-34", ]
interaction.plot(x.factor = esoph1$alcgp, trace.factor = esoph1$tobgp, response = esoph1$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 25-34")
#legend(x = 1, y = 1, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

#35-44
esoph2 <- esoph[esoph$agegp == "35-44", ]
interaction.plot(x.factor = esoph2$alcgp, trace.factor = esoph2$tobgp, response = esoph2$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 35-44")
#legend(x = 2.5, y = 3, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

#45-54
esoph3 <- esoph[esoph$agegp == "45-54", ]
interaction.plot(x.factor = esoph3$alcgp, trace.factor = esoph3$tobgp, response = esoph3$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 45-54")
#legend(x = 1.75, y = 2, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

#55-64
esoph4 <- esoph[esoph$agegp == "55-64", ]
interaction.plot(x.factor = esoph4$alcgp, trace.factor = esoph4$tobgp, response = esoph4$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 55-64")
#legend(x = .75, y = 9, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

#65-74
esoph5 <- esoph[esoph$agegp == "65-74", ]
interaction.plot(x.factor = esoph5$alcgp, trace.factor = esoph5$tobgp, response = esoph5$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 65-74")
#legend(x = 3, y = 15, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

#75+
esoph6 <- esoph[esoph$agegp == "75+", ]
interaction.plot(x.factor = esoph6$alcgp, trace.factor = esoph6$tobgp, response = esoph6$ncases, 
                 type = "b", xlab = "Daily Alcohol Intake (g)", pch = 1:4, lty = 1:4, col = c(1:4), # yaxt = "n", 
                 legend = FALSE, ylab = "Mean Number of Cases", main = "Interaction Plot for Ages 75+")
#legend(x = 1, y = .75, legend = c("0-9", "10-19","20-19","30+"), pch = 1:4, lty = 1:4, col = c(1:4), title = "Daily Tobacco Intake (g)")

######### create treatment combinations ######### 
esoph <- within(data = esoph,
              {	fA <- factor(alcgp);
              fB <- factor(tobgp);
              fC <- factor(agegp);
              fTC <- factor(as.numeric(factor(paste(alcgp, tobgp, agegp, sep = ""))))
              order <- factor(c(1:88))})

#########  unbalanced sum of squares ######### 
# needed for Type III sum of squares	
options(contrasts = c("contr.sum", "contr.poly")) 

#########  anova model with two-way interactions ######### 
esoph.lm <- aov(ncases ~  fA + fB + fC + fA:fB + fA:fC +fB:fC, data = esoph)
anova(esoph.lm)
# Type I sum of squares
anova(esoph.lm)
# Type III sum of squares
drop1(object = esoph.lm, scope = ~., test = "F")

#########  Simultaneous Confidence Intervals ######### 
#########  model without negligible interaction ######### 
esoph.lm <- aov(ncases ~ fA + fB + fC + fA:fC, data = esoph)
anova(esoph.lm)
#########  contrasts for alcohol (factor A) ######### 
esoph.lsmalcohol <- lsmeans(esoph.lm, ~ fA)
esoph.lsmalcohol
summary(contrast(object = esoph.lsmalcohol, method = "pairwise", adjust = "Tukey"),  level = 0.95, side = "two-sided", infer = c(T, T))

#########  contrasts for tobacco (factor B) ######### 
esoph.lsmtob <- lsmeans(esoph.lm, ~ fB)
esoph.lsmtob
summary(contrast(object = esoph.lsmtob, method = "pairwise", adjust = "Tukey"),  level = 0.95, side = "two-sided", infer = c(T, T))

#########  contrasts for age (factor C) ######### 
esoph.lsmage <- lsmeans(esoph.lm, ~ fC)
esoph.lsmage
summary(contrast(object = esoph.lsmage, method = "pairwise", adjust = "Tukey"),  level = 0.95, side = "two-sided", infer = c(T, T))


#########  obtain the residuals and predicted values ######### 
esoph.z <- residuals(esoph.lm) / sd(residuals(esoph.lm))
esoph.p <- fitted(esoph.lm)
par(mfrow = c(2, 2))
#########  check outliers and model fitting ######### 
plot(esoph.z ~ esoph$fA, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Alcohol Model Fit")
lines(x = c(-1, 19), y = c(0, 0))
plot(esoph.z ~ esoph$fB, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Tobacco Model Fit")
lines(x = c(-1, 19), y = c(0, 0))
plot(esoph.z ~ esoph$fC, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Age Model Fit")
lines(x = c(-1, 19), y = c(0, 0))
plot(esoph.z ~ esoph$fTC, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "All Treatment Combinations Model Fit")
lines(x = c(-1, 100), y = c(0, 0))
#########  check normality ######### 
qqnorm(y = esoph.z, xlab = "Theory Quantiles", ylab = "Residual Quantiles", main = "Normality")
qqline(y = esoph.z)
#########  checking equal variance assumption using plot ######### 
plot(esoph.z ~ esoph.p, xlab = "Predicted Values", ylab = "Standardized Residuals", main = "Equal Variance") 
lines(x = c(-1, 100), y = c(0, 0))
plot(esoph.z ~ esoph$fTC, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Outliers") 
lines(x = c(-1, 100), y = c(0, 0))
#########  residual plots for checking independence of error ######### 
plot(esoph.z ~ esoph$order, xlab = "Time Order", ylab = "Standardized Residuals", 
     main = "Independence") 
lines(x = c(-1, 100), y = c(0, 0))

#########  model without max standardized residual (outliers) ######### 

sse <- 612.5839
n<-88
sr <- esoph$ncases/sqrt(sse/(n-1))

esoph <- cbind(esoph, sr)
esoph.sort <- esoph[order(sr), ]

esoph.sort[1,]
esoph.sort[88,]
esoph.sort[87,]
esoph.sort[86,]

esoph.without.max.resid <- esoph.sort[1:85, ]

esoph.without.max.resid.lm <- aov(ncases ~ fA + fB + fC + fA:fC, data = esoph.without.max.resid)
anova(esoph.without.max.resid.lm)

#########  obtain the residuals and predicted values of model without max resid ######### 
esoph.without.max.resid.z <- residuals(esoph.without.max.resid.lm) / sd(residuals(esoph.without.max.resid.lm))
esoph.without.max.resid.p <- fitted(esoph.without.max.resid.lm)


#########  check normality ######### 
qqnorm(y = esoph.without.max.resid.z, xlab = "Theory Quantiles", ylab = "Residual Quantiles", main = "Normality Without Max Resid")
qqline(y = esoph.without.max.resid.z)
#########  checking equal variance assumption using plot ######### 
plot(esoph.without.max.resid.z ~ esoph.without.max.resid.p, xlab = "Predicted Values", ylab = "Standardized Residuals", main = "Equal Variance Without Max Resid") 
lines(x = c(-1, 100), y = c(0, 0))
plot(esoph.without.max.resid.z ~ esoph.without.max.resid$fTC, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Equal Variance Without Max Resid") 
lines(x = c(-1, 100), y = c(0, 0))
# residual plots for checking independence of error
plot(esoph.without.max.resid.z ~ esoph.without.max.resid$order, xlab = "Time Order", ylab = "Standardized Residuals", 
     main = "Independence Without Max Resid") 
lines(x = c(-1, 100), y = c(0, 0))



#Levene's and sample variance ratio can only be used for three-way interaction 
#leveneTest(ncases ~ factor(fA), data = esoph.without.max.resid, center = mean)
#leveneTest(ncases ~ factor(fB), data = esoph.without.max.resid, center = mean)
#leveneTest(ncases ~ factor(fC), data = esoph.without.max.resid, center = mean)
#leveneTest(ncases ~ factor(fA:fC), data = esoph.without.max.resid, center = mean)

#esoph.gv.alc <- tapply(X = esoph.without.max.resid$ncases, INDEX = esoph.without.max.resid$fA, FUN = var) # sample variance of treatment combinations
#max(esoph.gv.alc)/min(esoph.gv.alc)

#esoph.gv.tob <- tapply(X = esoph.without.max.resid$ncases, INDEX = esoph.without.max.resid$fB, FUN = var) # sample variance of treatment combinations
#max(esoph.gv.tob)/min(esoph.gv.tob)

#esoph.gv.age <- tapply(X = esoph.without.max.resid$ncases, INDEX = esoph.without.max.resid$fC, FUN = var) # sample variance of treatment combinations
#max(esoph.gv.age)/min(esoph.gv.age)

############ data transformation (not used) #####################

# new data: log +1 of original data
esoph.trans <- within(data = esoph.without.max.resid,
                      {	ncases <- log(esoph.without.max.resid$ncases+1);
                      })
esoph.trans.lm <- aov(ncases ~ fA + fB + fC + fA:fC, data = esoph.trans)
anova(esoph.trans.lm)
#########  obtain the residuals and predicted values of model after transformation ######### 
esoph.trans.z <- residuals(esoph.trans.lm) / sd(residuals(esoph.without.max.resid.lm))
esoph.trans.p <- fitted(esoph.trans.lm)

leveneTest(ncases ~ factor(fA), data = esoph.trans, center = mean)
leveneTest(ncases ~ factor(fB), data = esoph.trans, center = mean)
leveneTest(ncases ~ factor(fC), data = esoph.trans, center = mean)
leveneTest(ncases ~ factor(fA:fC), data = esoph.trans, center = mean)

esoph.gv.alc <- tapply(X = esoph.trans$ncases, INDEX = esoph.trans$fA, FUN = var) # sample variance of treatment combinations
max(esoph.gv.alc)/min(esoph.gv.alc)

esoph.gv.tob <- tapply(X = esoph.trans$ncases, INDEX = esoph.trans$fB, FUN = var) # sample variance of treatment combinations
max(esoph.gv.tob)/min(esoph.gv.tob)

esoph.gv.age <- tapply(X = esoph.trans$ncases, INDEX = esoph.trans$fC, FUN = var) # sample variance of treatment combinations
max(esoph.gv.age)/min(esoph.gv.age)

#########  check normality ######### 
qqnorm(y = esoph.trans.z, xlab = "Theory Quantiles", ylab = "Residual Quantiles", main = "Normality Data After Transformation")
qqline(y = esoph.trans.z)
#########  checking equal variance assumption using plot ######### 
plot(esoph.trans.z ~ esoph.trans.p, xlab = "Predicted Values", ylab = "Standardized Residuals", main = "Equal Variance After Data Transformation") 
lines(x = c(-1, 100), y = c(0, 0))
plot(esoph.trans.z ~ esoph.trans$fTC, xlab = "Treatment Levels", ylab = "Standardized Residuals", main = "Equal Variance After Data Transformation") 
lines(x = c(-1, 100), y = c(0, 0))
# residual plots for checking independence of error
plot(esoph.trans.z ~ esoph.trans$order, xlab = "Time Order", ylab = "Standardized Residuals", 
     main = "Independence After Data Transformation") 
lines(x = c(-1, 100), y = c(0, 0))

