library(tidyverse)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(grid)
library(gridExtra)

#FULL DATASET
cdata<-read_tsv('./GoogleDataProject/EntropyInputs/EntropyDatasetforRParentsOnly.txt')
cdata.11<-cdata %>% filter(year==2011)
cdata.12<-cdata %>% filter(year==2012)
cdata.13<-cdata %>% filter(year==2013)
cdata.14<-cdata %>% filter(year==2014)
cdata.15<-cdata %>% filter(year==2015)
cdata.16<-cdata %>% filter(year==2016)
cdata.17<-cdata %>% filter(year==2017)
cdata.18<-cdata %>% filter(year==2018)
cdata.19<-cdata %>% filter(year==2019)
cdata.20<-cdata %>% filter(year==2020)
gini.full<-ggplot(data=cdata.15,aes(x=factor(0),y=gini))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.16,aes(x=factor(1),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.16,aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.16,aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.17,aes(x=factor(2),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.17,aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.17,aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.18,aes(x=factor(3),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.18,aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.18,aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.19,aes(x=factor(4),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.19,aes(x=factor(4),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.19,aes(x=factor(4),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.20,aes(x=factor(5),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.20,aes(x=factor(5),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.20,aes(x=factor(5),y=gini),
               fun=median, geom="point", size=2, color="red")+
   scale_x_discrete(labels= 2015:2020)+
  scale_y_continuous(breaks=c(0,.25,.5,.75,1))+
  labs(y="Full Dataset\nGini Coefficient",x="")+
  guides(color = "none",alpha='none',fill='none')

###
cdata<-read_tsv('./GoogleDataProject/EntropyInputs/EntropyDatasetforRParentsOnly.txt')
cdata<-cdata %>% filter(CV==1)
cdata.11<-cdata %>% filter(year==2011)
cdata.12<-cdata %>% filter(year==2012)
cdata.13<-cdata %>% filter(year==2013)
cdata.14<-cdata %>% filter(year==2014)
cdata.15<-cdata %>% filter(year==2015)
cdata.16<-cdata %>% filter(year==2016)
cdata.17<-cdata %>% filter(year==2017)
cdata.18<-cdata %>% filter(year==2018)
cdata.19<-cdata %>% filter(year==2019)
cdata.20<-cdata %>% filter(year==2020)
gini.CV<-ggplot(data=cdata.15,aes(x=factor(0),y=gini))+
  geom_violin(fill='purple',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.16,aes(x=factor(1),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=cdata.16,aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.16,aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.17,aes(x=factor(2),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=cdata.17,aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.17,aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.18,aes(x=factor(3),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=cdata.18,aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.18,aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.19,aes(x=factor(4),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=cdata.19,aes(x=factor(4),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.19,aes(x=factor(4),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.20,aes(x=factor(5),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=cdata.20,aes(x=factor(5),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.20,aes(x=factor(5),y=gini),
               fun=median, geom="point", size=2, color="red")+
  scale_x_discrete(labels= 2015:2020)+
  scale_y_continuous(breaks=c(0,.25,.5,.75,1))+
  labs(y="Computer Vision\nGini Coefficient",x="")+
  guides(color = "none",alpha='none',fill='none')
#NLP
cdata<-read_tsv('./GoogleDataProject/EntropyInputs/EntropyDatasetforRParentsOnly.txt')
cdata<-cdata %>% filter(NLP==1)
cdata.11<-cdata %>% filter(year==2011)
cdata.12<-cdata %>% filter(year==2012)
cdata.13<-cdata %>% filter(year==2013)
cdata.14<-cdata %>% filter(year==2014)
cdata.15<-cdata %>% filter(year==2015)
cdata.16<-cdata %>% filter(year==2016)
cdata.17<-cdata %>% filter(year==2017)
cdata.18<-cdata %>% filter(year==2018)
cdata.19<-cdata %>% filter(year==2019)
cdata.20<-cdata %>% filter(year==2020)
gini.NLP<-ggplot(data=cdata.15,aes(x=factor(0),y=gini))+
  geom_violin(fill='orange',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.16,aes(x=factor(1),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=cdata.16,aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.16,aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.17,aes(x=factor(2),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=cdata.17,aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.17,aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.18,aes(x=factor(3),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=cdata.18,aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.18,aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.19,aes(x=factor(4),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=cdata.19,aes(x=factor(4),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.19,aes(x=factor(4),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.20,aes(x=factor(5),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=cdata.20,aes(x=factor(5),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.20,aes(x=factor(5),y=gini),
               fun=median, geom="point", size=2, color="red")+
  scale_x_discrete(labels= 2015:2020)+
  scale_y_continuous(breaks=c(0,.25,.5,.75,1))+
  labs(y="Natural Language Processing\nGini Coefficient",x="")+
  guides(color = "none",alpha='none',fill='none')

###METHODOLOGY
cdata<-read_tsv('./GoogleDataProject/EntropyInputs/EntropyDatasetforRParentsOnly.txt')
cdata<-cdata %>% filter(Methodology==1)
cdata.11<-cdata %>% filter(year==2011)
cdata.12<-cdata %>% filter(year==2012)
cdata.13<-cdata %>% filter(year==2013)
cdata.14<-cdata %>% filter(year==2014)
cdata.15<-cdata %>% filter(year==2015)
cdata.16<-cdata %>% filter(year==2016)
cdata.17<-cdata %>% filter(year==2017)
cdata.18<-cdata %>% filter(year==2018)
cdata.19<-cdata %>% filter(year==2019)
cdata.20<-cdata %>% filter(year==2020)
gini.Methodology<-ggplot(data=cdata.15,aes(x=factor(0),y=gini))+
  geom_violin(fill='red',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.16,aes(x=factor(1),y=gini),fill='red',alpha=.4)+
  geom_boxplot(data=cdata.16,aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.16,aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.17,aes(x=factor(2),y=gini),fill='red',alpha=.4)+
  geom_boxplot(data=cdata.17,aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.17,aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.18,aes(x=factor(3),y=gini),fill='red',alpha=.4)+
  geom_boxplot(data=cdata.18,aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.18,aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.19,aes(x=factor(4),y=gini),fill='red',alpha=.4)+
  geom_boxplot(data=cdata.19,aes(x=factor(4),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.19,aes(x=factor(4),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.20,aes(x=factor(5),y=gini),fill='red',alpha=.4)+
  geom_boxplot(data=cdata.20,aes(x=factor(5),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.20,aes(x=factor(5),y=gini),
               fun=median, geom="point", size=2, color="red")+
  scale_x_discrete(labels= 2015:2020)+
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),n.breaks=5)+
  labs(y="Methodology\nGini Coefficient",x="")+
  guides(color = "none",alpha='none',fill='none')
gini.time<-grid.arrange(gini.full,gini.CV,gini.NLP,gini.Methodology,nrow=4)



#FULL DATASET
cdata<-read_tsv('./GoogleDataProject/EntropyInputs/EntropyDatasetforRParentsOnly.txt')
cdata<-cdata %>% filter(task_size>=10)
cdata.11<-cdata %>% filter(year==2011)
cdata.12<-cdata %>% filter(year==2012)
cdata.13<-cdata %>% filter(year==2013)
cdata.14<-cdata %>% filter(year==2014)
cdata.15<-cdata %>% filter(year==2015)
cdata.16<-cdata %>% filter(year==2016)
cdata.17<-cdata %>% filter(year==2017)
cdata.18<-cdata %>% filter(year==2018)
cdata.19<-cdata %>% filter(year==2019)
cdata.20<-cdata %>% filter(year==2020)
gini.full<-ggplot(data=cdata.15,aes(x=factor(0),y=gini))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.16,aes(x=factor(1),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.16,aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.16,aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.17,aes(x=factor(2),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.17,aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.17,aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.18,aes(x=factor(3),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.18,aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.18,aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.19,aes(x=factor(4),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.19,aes(x=factor(4),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.19,aes(x=factor(4),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=cdata.20,aes(x=factor(5),y=gini),fill='green',alpha=.4)+
  geom_boxplot(data=cdata.20,aes(x=factor(5),y=gini),width=.1,alpha=.5)+
  stat_summary(data=cdata.20,aes(x=factor(5),y=gini),
               fun=median, geom="point", size=2, color="red")+
  scale_x_discrete(labels= 2015:2020)+
  scale_y_continuous(breaks=c(0,.25,.5,.75,1))+
  labs(y="Gini Coefficient\n(Higher = Less Dataset Diversity)",x="")+
  guides(color = "none",alpha='none',fill='none')


