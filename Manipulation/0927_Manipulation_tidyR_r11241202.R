rm(list=ls())

library (tidyr)

#Pivoting 

Tw.corals <-read.table('Data/tw_corals.txt', header=T, sep='\t', dec='.')
Tw.corals

#turn wide table to long table
Tw.corals_long <- Tw.corals %>% pivot_longer(Southern_TW:Northern_Is, names_to = "Region", values_to = "Richness")
Tw.corals_long 

#turn long table to wide table
Tw.corals_wide <- pivot_wider(Tw.corals_long, names_from = Region, values_from = Richness) 
Tw.corals_wide

income.world<-read.table('Data/metoo.txt',header=T, sep="\t", dec=".", na.strings = "n/a")
income.world

#rearrange variables by separating them
income.world_long <- income.world %>% pivot_longer(cols = -state, 
                                                   names_to = c("gender", "work"),
                                                   names_sep = "_",
                                                   values_to = "income") #separation is underbar,"_"
income.world_long

#you can also merge those back
income.world_long %>% pivot_wider(names_from = c(gender,work), 
                            values_from = income,
                            names_sep = ".")

#create a dummy variable for manipulation
income.world_long_var <- income.world %>%  pivot_longer(cols = -1, 
                                            names_to = "var1", 
                                            values_to = "income")
income.world_long_var

#split dummy "var1" to two columns
income.world_sep <- income.world_long_var %>%  separate(col = var1, 
                                            sep = "_", 
                                            into = c("gender", "work"))
income.world_sep

#also you can separate them in original column by split them in rows 
income.world_long_var %>% separate_rows(var1, sep = "_")

