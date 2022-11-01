xaringanExtra::use_clipboard()





library (psych)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)

# students data set url 
students<-read.table('D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/students.txt',header=T, sep="\t", dec='.') 
# to write it:
write.table(students, file = "D:/R_WD/Git linked/2022RisFUN-Mo/Statistics/Data-statistic/students.txt", sep = "\t", row.names=T)

summary(students)

psych::describe(students)

psych::describeBy (students,students$gender)
psych::describeBy (students, list(students$gender,students$population))

# One variables
c
prop.table (table(students$gender))

# two variables
table(students$gender, students$shoesize)
prop.table (table(students$gender, students$shoesize))

# three variables, with nicer formatting
ftable(students$gender, students$shoesize,students$population)

mean(students$height)
ind.male <- students$gender == 'male'
mean(students$height[ind.male])

aggregate(students$height,list (students$gender),median)
tapply(students$height,students$gender, median)

## plot1 <-ggplot(iris, aes(x=Species, y=Sepal.Length)) +
##   geom_boxplot()
## 
## plot2 <-ggplot(iris, aes(x=Species, y=Sepal.Width)) +
##   geom_boxplot()
## 
## plot3 <-ggplot(iris, aes(x=Species, y=Petal.Length)) +
##   geom_boxplot()
## 
## plot4 <-ggplot(iris, aes(x=Species, y=Petal.Width)) +
##   geom_boxplot()
## 
## grid.arrange(plot1, plot2,plot3, plot4, ncol=2)
## describeBy (iris, iris$Species)
## 
## iris %>% group_by(Species) %>% summarise(across(c(1:4), length))
## aggregate(iris[,1:4],by=list(iris$Species), median)
## tapply(iris$Sepal.Length , iris$Species, mean)

# dataset hypotheses?
x<-students$height
y<-students$shoesize
s<-students[,1:2] # a matrix
# Pearson correlation
# cor(x,y)
# cor(s)
cor.test(x,y)

ggplot(students, aes(x = height, y = shoesize)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Spearman correlation (monotonic)
# cor(x,y, method ='spearman')
cor.test(x,y, method ='spearman')

## w<-(1:100)
## z<-exp(x)
## cor.test(w,z,method='pearson') # super low
## cor.test(w,z,method='spearman') #super high

#Cast 240 times a die. We counted occurrence of 1,2,3,4,5,6
die<-data.frame(obs=c(55,44,35,45,31,30), row.names=c('Cast1','Cast2','Cast3','Cast4','Cast5','Cast6'))
die #Is this die fair? Define H0 and H1.  

chisq.test(die)
# I am cheating

obs <- c(750, 50, 200)
exp <- c(0.60, 0.35, 0.05)
chisq.test (x=obs, p=exp)

F <- matrix(nrow=4,ncol=2,data=c(33,14, 8,18,31,25,14,12))
chisq.test(F) # alternative see `fisher.test`

# One sample
t.test (students$height, mu=170)
# Two sample (with equal variances)
t.test (students$height~students$gender, var.equal = TRUE)
# Two sample (with unequal variances, default option when using t.test) 
t.test (students$height~students$gender)
# Two sample paired t.test
t.test (students$height~students$gender, paired=T)

## set <- iris[iris$Species == "setosa", ]$Sepal.Length
## ver <- iris[iris$Species == "versicolor", ]$Sepal.Length
## vir <- iris[iris$Species == "virginica", ]$Sepal.Length
## 
## setver <- t.test(set, ver, paired = FALSE, alternative = "two.sided", var.equal = FALSE)
## vervir <- t.test(ver, vir, paired = FALSE, alternative = "two.sided", var.equal = FALSE)

# Normality plot & test
students$height[6]<-132
students$height[10]<-310
students$height[8]<-132
students$height[9]<-210
boxplot(height~gender, students)
qqnorm(students$height) 
qqline(students$height) 
shapiro.test(students$height) # if data are not normal 
wilcox.test (students$height~students$gender) 

# Test of variance: we test HO: homogeneous, H1:heterogeneous
fligner.test (students$height ~ students$gender)

tg<-ToothGrowth
tg$dose<-factor(tg$dose)
boxplot(len~dose*supp, data=tg)
# also work with: boxplot(len ~ interaction (dose,supp), data=tg)
# or: plot(len ~ interaction (dose,supp), data=tg)
bartlett.test(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality +++
leveneTest(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality ++
fligner.test(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality +
