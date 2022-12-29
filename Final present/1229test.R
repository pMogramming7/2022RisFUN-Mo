control <- read.csv("ILUC_DARE_controls_x_y.csv")
campaign <- read.csv("ILUC_DARE_campaign_x_y.csv") #this file is too big
class(campaign$timestamp)
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

campaign.rqs0.8 <- campaign[which(campaign$rqs>=0.8),]
campaign.rqs0.8_pivot <- campaign.rqs0.8%>%pivot_wider(id_cols = c(sampleid,x,y),
                                       names_from = step,values_from = answer,
                                       values_fn = function(x)paste(x,collapse = ","))

#從這開始
control <- control%>%arrange(answer)
control_pivot <- control%>%pivot_wider(id_cols = c(sampleid,x,y),
                                       names_from = step,values_from = answer,
                                       values_fn = function(x)paste(x,collapse = ","))
control_pivot <- rename(control_pivot, architecture=step3,predominant.driver=step1,other.drivers=step2)
#有些sampleid 有多個step2

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
predominant.driver

x.range.s.America <- c(-120,-20)
y.range.s.America <- c(-45,40)
x.range.Africa <- c(-20,55)
y.range.Africa <- c(-35,23.5)
x.range.Asia <- c(63,160)
y.range.Asia <- c(-35,40)
s.America.predominant.plot <- ggplot(data=world)+  #south America
  geom_sf()+
  coord_sf(xlim=c(-120,-20),ylim=c(-45,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in south America")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
s.America.predominant.plot
s.America <- filter(control_pivot, x<=-20 & x>=-120 & y<=40 & y>=-45)


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
Africa <- filter(control_pivot, x<=55 & x>=-20 & y<=23.5 & y>=-35)


Asia.predominant.plot <- ggplot(data=world)+  #Asia
  geom_sf()+
  coord_sf(xlim=c(63,160),ylim=c(-35,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in Asia")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
Asia.predominant.plot
Asia <- filter(control_pivot, x<=160 & x>=63 & y<=40 & y>=-35)


levels(factor(control_pivot$predominant.driver))
levels(factor(control_pivot$other.drivers))
levels(factor(control_pivot[which(control_pivot$architecture=="No"),"other.drivers"]))
levels(factor(control_pivot[which(control_pivot$architecture=="No"),"predominant.driver"]))


predominant.driver <- fct_count(factor(control_pivot$predominant.driver),sort = T)
fct_count(factor(control_pivot$other.drivers),sort = T)

predominant.driver$ratio <- predominant.driver$n/sum(predominant.driver$n)
ratio.pre.driver <- ggplot(data = predominant.driver, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in global")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver

predominant.drivers.SAmerica <- fct_count(factor(s.America$predominant.driver),sort = T)
predominant.drivers.SAmerica$ratio <- predominant.drivers.SAmerica$n/sum(predominant.drivers.SAmerica$n)
ratio.pre.driver.sAmerica <- ggplot(data = predominant.drivers.SAmerica, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in South America")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver.sAmerica

predominant.drivers.Africa <- fct_count(factor(Africa$predominant.driver),sort = T)
predominant.drivers.Africa$ratio <- predominant.drivers.Africa$n/sum(predominant.drivers.Africa$n)
ratio.pre.drivers.Africa <- ggplot(data = predominant.drivers.Africa, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Africa")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Africa

predominant.drivers.Asia <- fct_count(factor(Asia$predominant.driver),sort = T)
predominant.drivers.Asia$ratio <- predominant.drivers.Asia$n/sum(predominant.drivers.Asia$n)
ratio.pre.drivers.Asia <- ggplot(data = predominant.drivers.Asia, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(name="Predominant drivers",palette = "Set1")+
  geom_text(aes(label=paste(100*round(ratio,3),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Asia")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Asia

barplot(c(predominant.drivers.SAmerica$ratio,predominant.drivers.Africa$ratio,predominant.drivers.Asia$ratio))

c17 <- c(
  "dodgerblue2", "#E31A1C","green4",
  "#6A3D9A","#FF7F00","black", "gold1",
  "skyblue2", "#FB9A99","palegreen2","#CAB2D6",
  "#FDBF6F", "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1")
other.drivers <- fct_count(factor(control_pivot$other.drivers),sort = T)
other.drivers$ratio <- other.drivers$n/sum(other.drivers$n)
ratio.other.drives <- ggplot(data = other.drivers, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Other drivers",values =c17) +
  geom_text(aes(label=paste(100*round(ratio,4),"%")),vjust=-0.3,size=3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of other tree loss driver in global")+
  labs(x="Other drivers",y="Ratio")
ratio.other.drives

architecture <- fct_count(factor(control_pivot$architecture),sort = T)
ratio.architecture <- architecture$n/sum(architecture$n) 
architecture.plot <- ggplot(data=world)+
  geom_sf()+
  coord_sf(expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,
                                    colour=factor(architecture,levels = c("Yes","No"))),
             alpha=0.9)+
  scale_color_manual(name="Architecture",values= c("Yes"="red","No"="green"))+
  theme_bw()+
  theme(legend.position = "bottom")
architecture.plot

control_pivot$region <- ifelse(control_pivot$x>=-120 & control_pivot$x<=-20,"South America",
                               ifelse(control_pivot$x>=-20 & control_pivot$x<=60,"Africa","Asia"))

#這裡要改
countries <- function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$ADMIN  #returns country name
}
points <- data.frame(lon=control_pivot$x,lat=control_pivot$y)
control_pivot$country <- countries(points)
presence.pre.drivers <- control_pivot%>%
 mutate(presence=1)%>%
 pivot_wider(id_cols = c(sampleid,x,y,region,country),names_from = predominant.driver,values_from = presence)
presence.pre.drivers[,-(1:5)] <-ifelse(is.na(presence.pre.drivers[,-(1:5)]),0,1)
#presence.pre.drivers.nmds <- metaMDS(presence.pre.drivers[,-(1:3)],distance = "bray",trymax = 1)
#plot(presence.pre.drivers.nmds)
#stressplot(presence.pre.drivers.nmds)

mds.predominant <- presence.pre.drivers%>%select(-(1:3))%>%
  vegdist(method = "bray")%>%
  cmdscale(eig=T,k=2)

as.tibble(mds.predominant$points)%>%
  bind_cols(Sample=presence.pre.drivers$sampleid)%>%
  ggplot()+
  geom_point(aes(x=V1,y=V2,col=control_pivot$region))

predominant.pca1 <- presence.pre.drivers[1:1000, ]%>%select(-(1:3))%>%
  prcomp(scale=T)
biplot(predominant.pca,scaling = 1)
biplot(predominant.pca)

#沒用國家去分
predominant.pca2 <- dudi.pca(presence.pre.drivers[-(1:5)],
                             scannf=F,nf=6,scale = F)
fviz_eig(predominant.pca2)
get_eig(predominant.pca2)
fviz_pca_biplot(predominant.pca2,repel = T,habillage=control_pivot$region,
                alpha.ind=0.5,addEllipses = T,ellipse.level=0.95,
                labelsize=3)+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))



predominant.pca3 <- dist.binary(presence.pre.drivers[-(1:3)],method = 1)
predominant.pca3.hc <- hclust(predominant.pca3)
plot(predominant.pca3.hc)

#用國家去分
presence.pre.drivers.na.omit <- presence.pre.drivers%>%na.omit(country)
presence.pre.drivers.countries <- presence.pre.drivers.na.omit%>%
  group_by(country)%>%
  summarise(across("Pasture":"Mining and crude oil extraction",.fns=sum))
#percentage
presence.pre.drivers.countries[-1] <- presence.pre.drivers.countries[-1]/rowSums(presence.pre.drivers.countries[-1]) 
countries.continent <- control_pivot%>% #國家跟洲對照表
  select(country,region)%>%
  unique()%>%
  na.omit()%>%
  arrange(country)


predominant.pca4 <- dudi.pca(presence.pre.drivers.countries[,-1],
                             scannf=F,nf=9,scale = F)
fviz_eig(predominant.pca4)
get_eig(predominant.pca4)
fviz_pca_biplot(predominant.pca4,repel = T,
                alpha.ind=0.5,
                labelsize=3,habillage = countries.continent$region,
                addEllipses = T,ellipse.level=0.95)+
  theme(text = element_text(size = 7.5),
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))
         