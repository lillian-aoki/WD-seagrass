library(tidyverse)
library(ggplot2)
library(lubridate)

region_order1 <- c("Alaska","British Colubmia","Washington","Oregon","Bodega Bay","San Diego")
region_order2 <- c("AK","BC","WA","OR","BB","SD")

## read-in data

DenCov <- read.csv("DensityCover.csv")
DenCov <- DenCov%>%
  separate(col=TransectID,sep="_",c("SampleDate","Region","SiteCode","Transect"))
DenCov$TidalHeight <- ifelse(DenCov$Transect=="L4"|DenCov$Transect=="L5"|DenCov$Transect=="L6","Lower","Upper")
DenCov$Region <- ordered(DenCov$Region,levels=region_order2)
ggplot(DenCov,aes(x=SiteCode,y=Density.shoots.m2,fill=TidalHeight))+geom_boxplot()+
  facet_wrap(~Region,scales = "free")+
  scale_fill_manual(values=c("royal blue","light blue"))

ggplot(DenCov,aes(x=SiteCode,y=Percent.Seagrass,fill=TidalHeight))+geom_boxplot()+
  facet_wrap(~Region,scales = "free")+
  scale_fill_manual(values=c("royal blue","light blue"))

ggplot(DenCov,aes(x=Region,y=Density.shoots.m2,fill=TidalHeight))+geom_boxplot()+
  scale_fill_manual(values=c("royal blue","light blue"))
