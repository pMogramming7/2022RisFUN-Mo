library (dplyr)

summarised <- summarise(iris, Mean.Width = mean(Sepal.Width))
head(summarised)

# by column names
selected1 <- dplyr::select(iris, Sepal.Length, Sepal.Width, Petal.Length)
head(selected1) 

# by column range
selected2 <- dplyr::select(iris, Sepal.Length:Petal.Length)
head(selected2, 4)

# by column range number
selected3 <- dplyr::select(iris,c(2:5))
head(selected3)

selected4 <- dplyr::select(iris, -Sepal.Length, -Sepal.Width)

# Select setosa species
filtered1 <- filter(iris, Species == "setosa" )
head(filtered1,3)

# Select versicolor species with Sepal width more than 3
filtered2 <- filter(iris, Species == "versicolor", Sepal.Width > 3)
tail(filtered2)

#To create a column “Greater.Half” which stores a logical vector
mutated1 <- mutate(iris, Greater.Half = Sepal.Width > 0.5 * Sepal.Length)
tail(mutated1)

table(mutated1$Greater.Half)

# Sepal Width by ascending order
arranged1 <- arrange(iris, Sepal.Width)
head(arranged1)

# Sepal Width by descending order
arranged2 <- arrange(iris, desc(Sepal.Width))
head(arranged2)

# Mean sepal width by Species
gp <- group_by(iris, Species)
gp.mean <- summarise(gp,Mean.Sepal = mean(Sepal.Width))
gp.mean

#To select the rows with conditions
iris %>% filter(Species == "setosa",Sepal.Width > 3.8)

iris  %>% group_by(Species) %>% summarise(Mean.Length = mean(Sepal.Length))
```{.r .distill-force-highlighting-css}
```
