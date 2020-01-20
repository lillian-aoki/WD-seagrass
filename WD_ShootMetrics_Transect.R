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
Shoots$EpibiontDryMass.g <- as.numeric(Shoots$EpibiontDryMass.g)

Shoots_summ1 <- Shoots %>%
  group_by(Region,SiteCode,TidalHeight,Transect)%>%
  summarise(LongestBladeLength.mm=mean(LongestBladeLength.mm,na.rm=TRUE),
            LongestBladeWidth.mm=mean(LongestBladeWidth.mm,na.rm=TRUE),
            SheathLength.mm=mean(SheathLength.mm,na.rm=TRUE),
            EpibiontDryMass.g=mean(EpibiontDryMass.g,na.rm=TRUE))

## canopy height
ggplot(Shoots_summ1,aes(x=Region,y=LongestBladeLength.mm,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Longest Blade Length (mm)")+
  labs(title="Canopy Height Summer 2019 by Region",
       subtitle = "Transect-level means, n=15-18 transects per tidal height per region")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(Shoots_summ1,aes(x=SiteCode,y=LongestBladeLength.mm,color=TidalHeight))+
  geom_point(position=position_dodge(width = 0.75))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  facet_wrap(~Region,scales = "fixed")+
  scale_color_manual(values=c("seagreen4","lightgreen"))+
  xlab("Site Code")+
  ylab("Longest Blade Length (mm)")+
  labs(title="Canopy Height Summer 2019 by Site",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

## leaf width
ggplot(Shoots_summ1,aes(x=Region,y=LongestBladeWidth.mm,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Longest Blade Width (mm)")+
  labs(title="Leaf Width Summer 2019 by Region",
       subtitle = "Transect-level means, n=15-18 transects per tidal height per region")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(Shoots_summ1,aes(x=SiteCode,y=LongestBladeWidth.mm,color=TidalHeight))+
  geom_point(position=position_dodge(width = 0.75))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  facet_wrap(~Region,scales = "fixed")+
  scale_color_manual(values=c("seagreen4","lightgreen"))+
  xlab("Site Code")+
  ylab("Longest Blade With (mm)")+
  labs(title="Leaf Width Summer 2019 by Site",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))


## sheath length
ggplot(Shoots_summ1,aes(x=Region,y=SheathLength.mm,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  #facet_wrap(~Region,scales = "fixed")+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Sheath length (mm)")+
  labs(title="Sheath Length Summer 2019 by Region",
       subtitle = "Transect-level means, n=15-18 transects per tidal height per region")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(Shoots_summ1,aes(x=SiteCode,y=SheathLength.mm,color=TidalHeight))+
  geom_point(position=position_dodge(width = 0.75))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  facet_wrap(~Region,scales = "fixed")+
  scale_color_manual(values=c("seagreen4","lightgreen"))+
  xlab("Site Code")+
  ylab("Sheath length (mm)")+
  labs(title="Sheath Length Summer 2019 by Site",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(Shoots,aes(x=SheathLength.mm,y=LongestBladeLength.mm,color=Region))+geom_point()+
  xlab("Sheath length (mm)")+
  ylab("Longest blade length (mm)")+
  labs(title="Canopy Height by Sheath Length 2019")+
  theme_bw()

## Epiphytes
ggplot(Shoots_summ1,aes(x=Region,y=EpibiontDryMass.g,fill=TidalHeight))+geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  #facet_wrap(~Region,scales = "fixed")+
  scale_fill_manual(values=c("seagreen4","lightgreen"))+
  xlab("Region")+
  ylab("Epibiont Dry Mass (g)")+
  labs(title="Epiphytes Summer 2019",
       subtitle = "Transect-level means, n=15-18 transects per tidal height per region")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))

ggplot(Shoots_summ1,aes(x=SiteCode,y=EpibiontDryMass.g,color=TidalHeight))+
  geom_point(position=position_dodge(width = 0.75))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=2, color="black",
               position=position_dodge(width = 0.75),show.legend = FALSE)+
  facet_wrap(~Region,scales = "fixed")+
  scale_color_manual(values=c("seagreen4","lightgreen"))+
  xlab("Site Code")+
  ylab("Epibiont Dry Mass (g)")+
  labs(title="Epiphytes Summer 2019",
       subtitle = "Transect-level means, n=3 transects per tidal height per site")+
  theme_bw()+
  theme(strip.background = element_rect(fill="white"))
