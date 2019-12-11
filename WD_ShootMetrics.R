library(tidyverse)
library(ggplot2)
library(lubridate)

region_order1 <- c("Alaska","British Colubmia","Washington","Oregon","Bodega Bay","San Diego")
region_order2 <- c("AK","BC","WA","OR","BB","SD")

## read-in data

Shoots <- read.csv("ShootMetrics.csv")
Shoots <- Shoots%>%
  separate(col=TransectID,sep="_",c("SampleDate","Region","SiteCode","Transect"))
Shoots$TidalHeight <- ifelse(Shoots$Transect=="L4"|Shoots$Transect=="L5"|Shoots$Transect=="L6","Lower","Upper")
Shoots$Region <- ordered(Shoots$Region,levels=region_order2)
ggplot(Shoots,aes(x=SiteCode,y=LongestBladeLength.mm,fill=TidalHeight))+geom_boxplot()+
  facet_wrap(~Region,scales = "free")+
  scale_fill_manual(values=c("royal blue","light blue"))

ggplot(Shoots,aes(x=SiteCode,y=SheathLength.mm,fill=TidalHeight))+geom_boxplot()+
  facet_wrap(~Region,scales = "fixed")+
  scale_fill_manual(values=c("royal blue","light blue"))

ggplot(Shoots,aes(x=Region,y=SheathLength.mm,fill=TidalHeight))+geom_boxplot()+
  #facet_wrap(~Region,scales = "fixed")+
  scale_fill_manual(values=c("royal blue","light blue"))
ggplot(Shoots,aes(x=Region,y=LongestBladeLength.mm,fill=TidalHeight))+geom_boxplot()+
  #facet_wrap(~Region,scales = "fixed")+
  scale_fill_manual(values=c("royal blue","light blue"))

ggplot(Shoots,aes(x=SheathLength.mm,y=LongestBladeLength.mm,color=Region))+geom_point()+
  theme_bw()
