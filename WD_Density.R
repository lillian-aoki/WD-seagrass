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
DenCov1 <- DenCov%>%
  group_by(Region,SiteCode,TidalHeight,Transect)%>%
  summarise(Density.shoots.m2=mean(Density.shoots.m2,na.rm = TRUE))

Den_summ1 <- DenCov1%>%
  group_by(Region,TidalHeight)%>%
  summarise(Density=mean(Density.shoots.m2),sd=sd(Density.shoots.m2),
            se=sd/sqrt(length(Density.shoots.m2)))
ggplot(Den_summ1,aes(x=Region,y=Density,fill=TidalHeight))+geom_col(position = "dodge")+
  geom_errorbar(aes(ymax=Density+2*se,ymin=Density-2*se),position=position_dodge(width=0.9),width=0.25)+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Shoot density (shoots m-2)")+
  labs(title="Shoot Density Summer 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

Den_summ2 <- DenCov1 %>%
  group_by(Region,SiteCode,TidalHeight)%>%
  summarise(Density=mean(Density.shoots.m2),sd=sd(Density.shoots.m2),
            se=sd/sqrt(length(Density.shoots.m2)))
ggplot(Den_summ2,aes(x=SiteCode,y=Density,fill=TidalHeight))+geom_col(position = "dodge")+
  geom_errorbar(aes(ymax=Density+2*se,ymin=Density-2*se),position=position_dodge(width=0.9),width=0.25)+
  facet_wrap(~Region,scales = "free")+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Shoot density (shoots m-2)")+
  labs(title="Shoot Density Summer 2019")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))


ggplot(DenCov,aes(x=SiteCode,y=Percent.Seagrass,fill=TidalHeight))+geom_boxplot()+
  facet_wrap(~Region,scales = "free")+
  scale_fill_manual(values=c("royal blue","light blue"))

