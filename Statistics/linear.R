xaringanExtra::use_clipboard()









library(Hmisc)
library(corrplot)
library(MASS)
library(car)
library(interactions)
library(yarrr)
library(tidyr)
library(readr)
library(lme4)
library (lmerTest)
library(nlme)
library(gvlma)

# correlation
rairuoho<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt',header=T, sep="\t", dec=".")
cor.test(rairuoho$day6, rairuoho$day7)

corr<-cor(rairuoho[,1:6])
corr # cor.test does not work on Matrix

p.val<-rcorr(as.matrix(rairuoho[,1:6]))$P
corrplot(corr,type='upper',method='color', addCoef.col = "black", p.mat=as.matrix(p.val), sig.level = 0.05,title = "Correlation Matrix", mar = c(2,0,2,0), diag=F)

plot(rairuoho$day6, rairuoho$day7)
abline(lm(rairuoho$day7~rairuoho$day6), col="red", lwd=2)
# remember `ggplot`
# ggplot(rairuoho, aes(x = day6, y = day7)) + 
#  geom_point() +
#  stat_smooth(method = "lm", col = "red")

model1 <- lm(Petal.Width ~ Petal.Length, data = iris)
model1$coefficients

ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(fill=Species),shape = 21, size=5) +
  stat_smooth(method = "lm", col = "blue")

summary(model1)

confint(model1)

sigma(model1)*100/mean(iris$Petal.Width)

fit1 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)
summary(fit1)

## # Other useful functions
## coefficients(fit1) # model coefficients
## confint(fit1, level=0.95) # CIs for model parameters
## fitted(fit1) # predicted values
## residuals(fit1) # residuals
## anova(fit1) # anova table
## vcov(fit1) # covariance matrix for model parameters
## influence(fit1) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit1)

# Assessing Outliers
outlierTest(fit1) # Bonferonni p-value for most extreme obs
qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit1) # leverage plots

# Influential Observations
# added variable plots
avPlots(fit1)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(iris)-length(fit1$coefficients)-2))
plot(fit1, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit1, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit1, main="QQ Plot")
# distribution of studentized residuals
sresid <- studres(fit1)
hist(sresid, freq=FALSE,
   main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit1)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit1)

# Evaluate Collinearity
vif(fit1) # variance inflation factors
sqrt(vif(fit1)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(fit1)
# Ceres plots
ceresPlots(fit1)

# Test for Autocorrelated Errors
durbinWatsonTest(fit1)

gvmodel <- gvlma(fit1)
summary(gvmodel)
