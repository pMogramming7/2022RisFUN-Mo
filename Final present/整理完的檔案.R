library(ggplot2)
library(ggmap)
library(rnaturalearth)
library(leaflet)
library(tidyverse)
library(forcats)
library(vegan)
library(RColorBrewer)
library(ade4)
library(factoextra)
library(magrittr)
library(sp)
library(rworldmap)

#載入檔案&整理
control <- read.csv("D:/R_WD/Git linked/2022RisFUN-Mo/Final present/Data_fp/ILUC_DARE_controls_x_y.csv")
control <- control%>%arrange(answer)
control_pivot <- control%>%pivot_wider(id_cols = c(sampleid,x,y),
                                       names_from = step,values_from = answer,
                                       values_fn = function(x)paste(x,collapse = ","))
control_pivot <- rename(control_pivot, architecture=step3,predominant.driver=step1,other.drivers=step2)

#做地圖
world <- ne_countries(scale = "medium", returnclass = "sf")
predominant.driver <- ggplot(data=world)+
  geom_sf()+
  coord_sf(expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.7,size=0.7)+
  ggtitle("Predominant drivers of global forest loss")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
predominant.driver #主要因子的世界分布圖

SAmerica.predominant.plot <- ggplot(data=world)+  #America
  geom_sf()+
  coord_sf(xlim=c(-120,-20),ylim=c(-45,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in America")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
SAmerica.predominant.plot



Africa.predominant.plot <- ggplot(data=world)+  #Africa
  geom_sf()+
  coord_sf(xlim=c(-20,55),ylim=c(-35,23.5),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in Africa")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
Africa.predominant.plot

AO.predominant.plot <- ggplot(data=world)+  #Asia
  geom_sf()+
  coord_sf(xlim=c(63,160),ylim=c(-35,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in Asia & Oceania")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
AO.predominant.plot

#三大洲的範圍 用於各大洲的柱狀圖使用
Am_bar <- filter(control_pivot, x<=-20 & x>=-120 & y<=40 & y>=-45)
Af_bar <- filter(control_pivot, x<=55 & x>=-20 & y<=23.5 & y>=-35)
AO_bar <- filter(control_pivot, x<=160 & x>=63 & y<=40 & y>=-35)

#主要因子的比例
predominant.driver <- fct_count(factor(control_pivot$predominant.driver),sort = T)
predominant.driver$ratio <- predominant.driver$n/sum(predominant.driver$n)
ratio.pre.driver <- ggplot(data = predominant.driver, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in global")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver #主要因子比例的柱狀圖 全世界

predominant.drivers.SAmerica <- fct_count(factor(Am_bar$predominant.driver),sort = T)
predominant.drivers.SAmerica$ratio <- predominant.drivers.SAmerica$n/sum(predominant.drivers.SAmerica$n)
ratio.pre.driver.sAmerica <- ggplot(data = predominant.drivers.SAmerica, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in America")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver.sAmerica #南美洲的柱狀圖

predominant.drivers.Africa <- fct_count(factor(Af_bar$predominant.driver),sort = T)
predominant.drivers.Africa$ratio <- predominant.drivers.Africa$n/sum(predominant.drivers.Africa$n)
ratio.pre.drivers.Africa <- ggplot(data = predominant.drivers.Africa, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Africa")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Africa #非洲的柱狀圖

predominant.drivers.Asia <- fct_count(factor(AO_bar$predominant.driver),sort = T)
predominant.drivers.Asia$ratio <- predominant.drivers.Asia$n/sum(predominant.drivers.Asia$n)
ratio.pre.drivers.Asia <- ggplot(data = predominant.drivers.Asia, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Asia & Oceania")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Asia #亞洲的柱狀圖

#將資料點以其位於的大洲做分類
control_pivot$region <- ifelse(control_pivot$x>=-120 & control_pivot$x<=-20,"South America",
                               ifelse(control_pivot$x>=-20 & control_pivot$x<=60,"Africa","Asia"))

countries <- function(points) #此函式用於將資料點以國家做分類
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$ADMIN  #returns country name
}
#將各資料點以國家做分類
points <- data.frame(lon=control_pivot$x,lat=control_pivot$y) #資料點的經緯度
control_pivot$country <- countries(points) #使用countries function

#將control_pivot dataframe以各資料點的主要因子做出presence absence matrix
presence.pre.drivers <- control_pivot%>% #presence absence matrix
  mutate(presence=1)%>%
  pivot_wider(id_cols = c(sampleid,x,y,region,country),names_from = predominant.driver,values_from = presence)
presence.pre.drivers[,-(1:5)] <-ifelse(is.na(presence.pre.drivers[,-(1:5)]),0,1) #1= presence, 0=absence

#不以國家，僅以資料點的經緯度做pca (看要不要放這個 pca圖比較醜)
predominant.pca2 <- dudi.pca(presence.pre.drivers[-(1:5)],
                             scannf=F,nf=6,scale = T)
fviz_eig(predominant.pca2) 
get_eig(predominant.pca2)
fviz_pca_biplot(predominant.pca2,repel = T,habillage=control_pivot$region,
                alpha.ind=0.5,addEllipses = T,ellipse.level=0.95,
                labelsize=3)+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))

#用國家去分
presence.pre.drivers.na.omit <- presence.pre.drivers%>%na.omit(country) #有些資料點無法定位在哪個國家內，而是以NA表示，並且因為只有8個資料點為NA，故將其排除不分析
presence.pre.drivers.countries <- presence.pre.drivers.na.omit%>% #以國家做分類後，將每個國家的資料點總合起來
  group_by(country)%>%
  summarise(across("Pasture":"Mining and crude oil extraction",.fns=sum))


countries.continent <- control_pivot%>% #國家與其位於之大洲對照表
  select(country,region)%>%
  unique()%>%
  na.omit()%>%
  arrange(country)

#以國家為單位做PCA
predominant.pca <- dudi.pca(presence.pre.drivers.countries[,-1],
                             scannf=F,nf=9,scale = T)
fviz_eig(predominant.pca)
get_eig(predominant.pca)
fviz_pca_biplot(predominant.pca,repel = T,
                alpha.ind=0.5,
                labelsize=3,habillage = countries.continent$region, #以三大洲做分組
                addEllipses = T,ellipse.level=0.95)+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))

