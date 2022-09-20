rm(list=ls())# clean memory
#call out library
library(datasets) #this is default packages in R

#remove existing list(all things in environment)
rm(list = ls())

data(iris) # import dataset 
# 'data' automatically  creates the object 'iris'
head (iris) # visualize 'head' dataset 只看前幾行

summary(iris) #  object summary
#including 中位數、四分衛數、平均值

str(iris) #可以看組成和性質

fix(iris) # spreadsheet 另開視窗可以直接改資料

#用網路資料來分析
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url
str(students)


#選column
students$height


#subset
students$gender=="female" # condition 抓出性別那行是female的
f<-students$gender=="female" # filter 把符合條件的裝到f
females<-students[f,] # selection 資料student中選f那列，f只符合剛剛設的條件
females
#將females這個資料框加入列名
rownames(females)<-c('Vanessa', 'Vicky', 'Michelle', 'Joyce', 'Victoria')
females

#Sample(隨機抽樣)隨機選一範圍的數字(類似抽籤，抽幾個)
rs <- sample(1:nrow(females), 3)
females[rs,] #秀出剛剛符合條件(rs)的


#Sorting
#order()排名的概念，預設排小到大，出現的數字代表原本列號
or <- order(students$height)
students[or,]
orr <- order(-students$height) #加負號變排大到小
students[orr,]
order(students$height,decreasing =T) #跟上面加負號一樣


#Recording 這邊老師用判斷式ifelse，並且把TRUE的變成blue,FALSE的變成red
colors<-ifelse(students$gender=='male', 'blue','red') 
colors
# create a new column-colors
students$colors<-ifelse(students$gender=='male','green','yellow') 
# replace an existing column
students$gender<-ifelse(students$gender=='male', 'blue','red') 


#Practice2.2
#create a new variable with flower colors
iris$color <- ifelse(iris$Species == 'setosa','purple',ifelse(iris$Species == 'versicolor','blue','pink')) 
iris

#Sort individuals by decreasing Sepal.Width.
sort <- order(iris$Sepal.Width, decreasing = T)
BtoS <- iris[sort,] #把order的結果帶回去iris重新排順序
BtoS[nrow(iris),] #叫出最後一個row

#having the smallest sepal width
min(iris$Sepal.Width) #得知是2
min2<-iris$Sepal.Width==2
iris[min2,]

irisordered<-order(-iris$Petal.Length)

#Practice2.1
#setosa
s <- iris$Species == 'setosa'
catseto <- iris[s,]

#versicolors 
ve<- iris$Species == 'versicolor'
catversi <- iris[ve,]

#virginica
vi<- iris$Species == 'virginica'
catvirg <- iris[vi,]
head(catversi)


subset(catversi[,1:5]) #for homework
# catversi$color<-NULL # (this will let "color column" lost, Be Careful!!!)
head(catversi)

callseto<- data.frame(matrix(ncol=0,nrow=50))
callseto$Sepal.Length= catseto$Sepal.Length
callseto$Petal.Length= catseto$Petal.Length
callseto$color= catseto$color
callseto
