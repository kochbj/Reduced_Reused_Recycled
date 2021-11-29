library(tidyverse)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library('ggrepel')
library('betareg')
library('ggeffects')
library('gridExtra')

#### LOAD DATA ####

#AFFILIATIONS
theme_set(theme_gray())
setwd("C:/Users/berna/Documents/Github/Life_of_a_Benchmark")
datasets<-read_csv('./Dataset_Curation/fullPWC_ginis_datasets.txt')%>%distinct(year,.keep_all=T)%>% arrange(year)
affiliations<-read_csv('./Dataset_Curation/fullPWC_ginis_institutions.txt')%>%distinct(year,.keep_all=T)%>% arrange(year)

#affiliation_years<-2012:2020
#affiliation_ginis<-c(0.65299547, 0.61567038, 0.68419941, 0.73988278, 0.7708926,0.79232109, 0.83122725, 0.84545099, 0.86220901)
#affiliation_simpsons<-c(0.21668363, 0.31951423, 0.2649137 , 0.18746646, 0.1393012 ,0.11548326, 0.08029687, 0.07240494, 0.05953283)
#usage_sizes<-c(261,   470,  1052,  2477,  4184,  7154, 12942, 19015, 25808)
#aff_ginis<-as.data.frame(cbind(affiliation_years,affiliation_ginis,usage_sizes))
#aff_ginis$years<-scale(affiliation_years,scale=F,center=T)
#aff_ginis$size<-scale(usage_sizes,scale=F,center=T)


#### Figure 3 Right ####

#Residualize usages on growth of PWC in case this is a confounder 
pwc_size_resid<-resid(lm(log(total)~total,distinct(datasets[c("year","total")])))
pwc_size_resid<-cbind(pwc_size_resid,distinct(datasets['year']))
datasets<-merge(datasets,pwc_size_resid,by='year')

pwc_size_resid<-resid(lm(log(total)~total,distinct(affiliations[c("year","total")])))
pwc_size_resid<-cbind(pwc_size_resid,distinct(affiliations['year']))
affiliations<-merge(affiliations,pwc_size_resid,by='year')


beta.aff<-betareg("gini~year+pwc_size_resid",data=affiliations)
mydf <- ggemmeans(beta.aff, terms = "year",ci.lvl=.95)
beta.dat<-betareg("gini~year+pwc_size_resid",data=datasets)
mydf.dat <- ggemmeans(beta.dat, terms = "year",ci.lvl=.95)
aff.diversity<-ggplot(mydf, aes(x, predicted,color='Institutions')) +
  geom_line() +
  geom_point(size = affiliations$total/max(affiliations$total)*12)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,color='Institutions',fill='Institutions'), alpha = .1,color=NA)+
  scale_y_continuous(name="Gini Coefficient (Higher = Less Diverse)") +
  scale_x_continuous(name="Year",breaks=2011:2020)+
  guides(color = "none")+
  theme(plot.title = element_text(hjust = 0.5),legend.position = c(1, 0),legend.justification = c(1, 0))+
  geom_line(data=mydf.dat,aes(x, predicted,color='Datasets'))+
  geom_ribbon(data=mydf.dat,aes(ymin = conf.low, ymax = conf.high,color='Datasets',fill='Datasets'), alpha = .1,color=NA) +
  geom_point(data=mydf.dat,aes(x, predicted,color='Datasets'),size = datasets$total/max(datasets$total)*12,fill='black')+
  scale_color_manual(name = "TEMP",breaks=c("Institutions","Datasets"),values=c("Institutions"='red',"Datasets"='black'))+
  scale_fill_manual(name = "",breaks=c("Institutions","Datasets"),values=c("Institutions"='red',"Datasets"='black'))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=18))

ggsave('./Figures/Figure3Right.png',aff.diversity,width=2000,height=2200,unit='px')
ggsave('./Figures/Figure3Right.svg',aff.diversity,width=2000,height=2200,unit='px')


#### Figure 3 Left ####

map_data<-read_tsv('./Dataset_Curation/map_data.tsv')
map_data$latitude<-as.numeric(map_data$Dataset_AffLatitude_Last)
map_data$longitude<-as.numeric(map_data$Dataset_AffLongitude_Last)
map_data$labeled<- ifelse(map_data$cumpct<.51,map_data$Dataset_AffDisplayName_Last,NA)
map_data$corporation<-as.factor(map_data$corporation)
theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
map.diversity<-ggplot(data = world) +
  geom_sf(alpha=.5) +
  geom_point(data = map_data, aes(x = longitude, y = latitude,color=corporation,alpha=.005), size = (map_data$usages/max(map_data$usages)+.1)*7)+
  scale_color_manual(values=c('orange','blue'),name='Corporate\nInstitution',labels=c("False","True"))+

  guides(alpha="none")+
  # geom_text_repel(data = map_data, aes(x = longitude, y = latitude, label = labeled,),
  #                 fontface = "bold",min.segment.length = 0,
  #                 nudge_x=1,nudge_y=1,
  #                 segment.color = "black",
  #                 color = "white",     # text color
  #                 bg.color = "grey30", # shadow color
  #                 bg.r = 0.15,
  #                 segment.size=1
  #                 )+
  ggtitle("Dataset-Introducing Institutions by Usage (June 2021)")+
    theme(plot.title = element_blank(),
          axis.text=element_blank(),
          axis.title=element_blank(),
          legend.position = c(1, 1), 
          legend.justification = c(1, 1),
          legend.title = element_blank(),
          legend.text = element_blank())

ggsave('./Figures/Figure3Left.png',map.diversity,width=3000,height=4000,unit='px')
ggsave('./Figures/Figure3Left.png',map.diversity,width=3000,height=4000,unit='px')