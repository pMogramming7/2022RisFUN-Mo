library(dplyr)
iris

#Summarize
M.Wsum <- summarise(iris, Mean.Width = mean(Sepal.Width))
head(M.Wsum)

#Select
column.select1 <- dplyr::select(iris, Sepal.Length, Sepal.Width, Petal.Length) #select specific columns
head(column.select1)

column.select2 <- dplyr::select(iris, Sepal.Length:Petal.Length) #select range column
head(column.select2, 4) #number after comma is how much data in your selected you want to view

column.select3 <- dplyr::select(iris, c(2:5)) #select column range by numbers
head(column.select3) #if not specify, it shows first six data in the data set

column.exclude1 <- dplyr::select(iris, -Sepal.Length, -Sepal.Width) #hide specific column by "-" mark
head(column.exclude1)

#Filter
filtred.one <- filter(iris, Species == "setosa") # "==", means: equals to
head(filtred.one, 3)

filtered.duo <- filter(iris, Species == "versicolor", Sepal.Width > 3) #multiple condition filtration
tail(filtered.duo, 2) #show last 2 row
filtered.duo[c(3:6), c(1,3)]

#Mutate
mutated.new <- mutate(iris, Greater.Half = Sepal.Width > 0.5 * Sepal.Length) #create a column "Greater.Half" which stores a logical vector
tail(iris)
tail(mutated.new)

table(mutated.new$Greater.Half)

#Arrange
arranged.ascen <- arrange(iris, Sepal.Width) #ascending order by Sepal Width
head(arranged.ascen, 3)
tail(arranged.ascen, 2)

arranged.descen <- arrange(iris, desc(Petal.Width)) #descending order by Petal.Width
tail(arranged.descen, 5)
head(arranged.descen, 4)

#Group_by
group.spe <- group_by(iris, Species) #group data by species
summarise(group.spe)
group.mean <- summarise(group.spe, Mean.Sepal = mean(Sepal.Width)) #group data summaried information of sepal width
group.mean

#Pipe operator
iris.fil <- iris %>% filter(Species == "setosa",Sepal.Width > 3.8)
iris %>% group_by(Species) %>% summarise(Mean.SL =mean(Sepal.Length)) #if you don't want to save the function result, don't give it a name ("name" <- function)




