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
library(simpleboot)
library(DAAG)
library(bootstrap)
library(MBESS)
library(leaps)

# correlation
rairuoho<-read.table('D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/rairuoho.txt',header=T, sep="\t", dec=".")
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

corr<-cor(iris[,1:3])
p.val<-rcorr(as.matrix(iris[,1:3]))$P
corrplot(corr,type='upper',method='color', addCoef.col = "black",  p.mat=as.matrix(p.val), sig.level = 0.05,diag=F)

# Evaluate Collinearity
car::vif(fit1) # variance inflation factors
sqrt(car::vif(fit1)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot
crPlots(fit1)
# Ceres plots
ceresPlots(fit1)

# Test for Autocorrelated Errors
durbinWatsonTest(fit1)

gvmodel <- gvlma(fit1)
summary(gvmodel)

boxcoxfit <- boxcox(fit1,plotit=T)
# also see `boxCox` from **car**
# also see `BoxCoxTrans` from **caret** for individual variable

lambda <- boxcoxfit$x[which.max(boxcoxfit$y)]

#fit new linear regression model using the Box-Cox transformation
new_fit <- lm(((iris$Petal.Width^lambda-1)/lambda) ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)

## gvlma(new_fit)
## # not better at all

set.seed(2021)
n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)
population.data <- as.data.frame(cbind(x, y))

sample.data <- population.data[sample(nrow(population.data), 20, replace = TRUE),]

population.model <- lm(y~x, population.data)
summary (population.data)

sample.model <- lm(y~x, sample.data)
summary (sample.model)

#Plotting the models
plot(y ~ x, col = "gray", main = 'Population and Sample Regressions')
abline(coef(population.model)[1], coef(population.model)[2], col = "red")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "blue",
       lty = 2)
legend(
  "topleft",
  legend = c("Sample", "Population"),
  col = c("red", "blue"),
  lty = 1:2,
  cex = 0.8)

# Containers for the coefficients
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:1000) {
  #Creating a resampled dataset from the sample data
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]
  
  #Running the regression on these data
  model_bootstrap <- lm(y ~ x, data = sample_d)
  
  #Saving the coefficients
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])
  
  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}

coefs <- rbind(sample_coef_intercept, sample_coef_x1)

# Combining the results in a table
means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1))
knitr::kable(round(
  cbind(
    population = coef(summary(population.model))[, 1],
    sample = coef(summary(sample.model))[, 1],
    bootstrap = means.boot),4), 
  "simple", caption = "Coefficients in different models")

a <-
  cbind(
    quantile(sample_coef_intercept, prob = 0.025),
    quantile(sample_coef_intercept, prob = 0.975))
b <-
  cbind(quantile(sample_coef_x1, prob = 0.025),
        quantile(sample_coef_x1, prob = 0.975))

c <-
  round(cbind(
    population = confint(population.model),
    sample = confint(sample.model),
    boot = rbind(a, b)), 4)
colnames(c) <- c("2.5 %", "97.5 %",
                 "2.5 %", "97.5 %",
                 "2.5 %", "97.5 %")
knitr::kable(rbind(
  c('population',
    'population',
    'sample',
    'sample',
    'bootstrap',
    'bootstrap'),c),caption = "Precision of the confidence intervals")


#Predicting on new data
new.data = seq(min(x), max(x), by = 0.05)
conf_interval <-
  predict(
    sample.model,
    newdata = data.frame(x = new.data),
    interval = "confidence",
    level = 0.95)

#Plotting the results on the project step-by-spep
plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))

plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))
abline(coef(population.model)[1], coef(population.model)[2], col = "blue")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "black",
       lty = 2, lwd=3)
abline(mean(sample_coef_intercept),
       mean(sample_coef_x1),
       col = "green",
       lty = 4, lwd=3)

plot(
  y ~ x,
  col = "gray",
  xlab = "x",
  ylab = "y",
  main = "Compare regressions")
apply(coefs, 2, abline, col = rgb(1, 0, 0, 0.03))
abline(coef(population.model)[1], coef(population.model)[2], col = "blue")
abline(coef(sample.model)[1],
       coef(sample.model)[2],
       col = "black",
       lty = 2, lwd=3)
abline(mean(sample_coef_intercept),
       mean(sample_coef_x1),
       col = "green",
       lty = 4, lwd=3)
lines(new.data, conf_interval[, 2], col = "black", lty = 3, lwd=3)
lines(new.data, conf_interval[, 3], col = "black", lty = 3, lwd=3)
legend("topleft",
       legend = c("Bootstrap", "Population", 'Sample'),
       col = c("red", "blue", 'green'),
       lty = 1:3,
       cex = 0.8)

fit1 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris) # sqrt(vif(fit1)) >2 not good
fit2 <- lm(Petal.Width ~ Sepal.Width + Sepal.Length, data = iris)
sqrt(car::vif(fit2)) >2 # okay
gvlma(fit2)
boxcoxfit2 <- boxcox(fit2,plotit=T)
lambda2 <- boxcoxfit2$x[which.max(boxcoxfit2$y)]
new_fit2 <- lm(((iris$Petal.Width^lambda2-1)/lambda2) ~  Sepal.Width + Sepal.Length, data = iris)
gvlma(new_fit2)

fit2.boot<-lm.boot(fit2, R=1000)
summary(fit2.boot)

# can specify casewise or residual sampling with the method argument
fit3.boot <- Boot(fit2, f=coef, R=1000, method=c("case"))
summary(fit3.boot, high.moments=T)

# K-fold cross-validation
cv.lm(iris, fit2, m=3) # 3 fold cross-validation

# Assessing R2 shrinkage using 10-Fold Cross-Validation
# define functions
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
# matrix of predictors
X <- as.matrix(iris[c("Sepal.Width","Sepal.Length")])
# vector of predicted values
y <- as.matrix(iris[c("Petal.Width")])
results <- crossval(X,y,theta.fit,theta.predict,ngroup=3)
cor(y, fit2$fitted.values)**2 # raw R2
cor(y,results$cv.fit)**2 # cross-validated R2

library("MBESS")
data(prof.salary)

fit.prof1 <- lm(salary~pub, data=prof.salary)
fit.prof2  <- lm(salary~citation, data=prof.salary)
fit.prof3 <- lm(salary~pub+citation, data=prof.salary)
fit.prof4  <- lm(salary~citation+pub, data=prof.salary)

# compare model 3 to model 1 - stepping approach, evaluating a new variable (cits)
anova(fit.prof1,fit.prof3)# note this is anova, not Anova

# compare model 3 to model 2 - stepping approach, evaluating a new variable (pubs)
anova(fit.prof2,fit.prof3)# note this is anova, not Anova

BIC(fit.prof1); BIC(fit.prof2);BIC(fit.prof3)
AIC(fit.prof1); AIC(fit.prof2);AIC(fit.prof3)

# Stepwise Selection based on AIC
fit.prof5  <- lm(salary~citation+pub+time, data=prof.salary)
step1 <- stepAIC(fit.prof5, direction="both")
step1$anova # display results


#Stepwise Selection with BIC
n<-dim(prof.salary)[1]
step2 <- stepAIC(fit.prof5, k=log(n), direction="both")
step2$anova # display results

# All Subsets Regression
leaps<-regsubsets(salary~citation+pub+time, data=prof.salary,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
# subsets(leaps, statistic="rsq")

fit.prof6  <- lm(salary~citation+time, data=prof.salary)
# gvlma(fit.prof6)
summary(fit.prof6)

summary(fit.prof6)$coefficient

confint(fit.prof6)

sigma(fit.prof6)/mean(prof.salary$salary)

## model.full <- lm(salary ~., data = prof.salary)

## model.partial1 <- lm(salary ~. -sex, data = prof.salary) # salary~".", the "." means all of the variables in the dataset

## model.partial2 <-  update(model.full,  ~. -sex)
