library(tidyverse)
library(emmeans)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(grid)
library(gridExtra)
library(performance)
#Gini
#could do exactly 0 with probabilty and then some other. Function would be beta distribution + spike and slab and then divide by 2
#Could have three parameter and then parameter of 0 is then density of beta. For any point exactly 0 density is .5. If not 0> beta density /2
#Could do likelihood test to see if its best

#How to check if task random intercepts dont remove all signal through time. If there's more intercepts than date stuff could get all the signal.
#If you use a single intercept see if likelihood becomes significantly shitty.Then put strong prior on intercepts but exponential prior on precision.

#Age could have its own effect.Then you could take residual. Could look at size relative to size (aka divide by mean of sizes in that age).


setwd("C:/Users/berna/Documents/Github/Life_of_a_Benchmark")
cdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsOnly.txt')
#cdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParents.NotMedians.txt')
#cdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsandChildren.txt')

cdata$clusters<-as.factor(cdata$task)
cdata<-cdata %>% filter(year<2021) %>% filter(year>2014)
cdata$date<-cdata$year-min(cdata$year)
cdata<-cdata %>% filter(task_size>=10)


cdata$task_age<-scale(cdata$task_age,center=T,scale=T)
cdata$full_size<-scale(cdata$pwc_size,center=T,scale=T)
cdata$task_size_n<-scale(cdata$task_size/cdata$pwc_size,center=T,scale=T)
cdata$task_size<-scale(cdata$task_size,center=T,scale=T)

pwc_size_resid<-resid(lm(log(pwc_size)~date,distinct(cdata[c("date","pwc_size")])))
pwc_size_resid<-cbind(pwc_size_resid,distinct(cdata['date']))
cdata<-merge(cdata,pwc_size_resid,by='date')

cdata$CV<-as.factor(cdata$CV)
cdata$NLP<-as.factor(cdata$NLP)
cdata$methods<-as.factor(cdata$Methodology)
cdata$gini_n<-ifelse(cdata$gini==0,cdata$gini+.00001,cdata$gini)
cdata$gini_n2<-(cdata$gini*(length(na.omit(cdata$gini))-1)+.5)/length(na.omit(cdata$gini))
cdata$pielou_n2<-(cdata$pielou*(length(na.omit(cdata$pielou))-1)+.5)/length(na.omit(cdata$pielou))

mlm.gini_0_full<-glmmTMB(gini_n2 ~ date*task_size*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.gini_1<-glmmTMB(gini_n2 ~ date*task_size+date*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.gini_2<-glmmTMB(gini_n2 ~ date*task_size+date+task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.gini_3<-glmmTMB(gini_n2 ~ date+task_size+date*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.gini_4<-glmmTMB(gini_n2 ~ date+task_size+date+task_age+CV*date+NLP*date+methods*date, data=cdata,family=beta_family(link = "logit"))
compare_performance(mlm.gini_0_full, mlm.gini_1, mlm.gini_2,mlm.gini_3,mlm.gini_4)

mylist <- list(task_size = quantile(cdata$task_size,p=c(.1,.25,.5,.75,.9)),date=1:(max(cdata$year)-min(cdata$year)+1),task_age=1:(max(cdata$year)-min(cdata$year)+1))
#mlm.gini_0_full<-mlm.gini_n2_full
gini.custom<-emmip(mlm.gini_2,NLP+CV+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
gini.custom<-gini.custom %>% filter(!(CV==1)) %>% filter(!(methods==1))
gini.custom$NLP <- factor(gini.custom$NLP, levels=c(1,0))
levels(gini.custom$NLP)<-c("Natural Language Processing","No Domain Fixed Effects")
gini.custom.NLP <- ggplot(data=gini.custom, aes(x=date,y=yvar, color=NLP)) +
  scale_color_manual(values=c("orange","green"))+
  scale_fill_manual(values=c("orange","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=NLP), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold"),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))+
  labs(title="",x="Year",y='',fill="")+
  guides(color = "none")

gini.custom<-emmip(mlm.gini_2,CV+NLP+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
gini.custom<-gini.custom %>% filter(!(NLP==1)) %>% filter(!(methods==1))
gini.custom$CV <- factor(gini.custom$CV, levels=c(1,0))
levels(gini.custom$CV)<-c("Computer Vision","No Domain Fixed Effects")
gini.custom.CV <- ggplot(data=gini.custom, aes(x=date,y=yvar, color=CV)) +
  scale_color_manual(values=c("purple","green"))+
  scale_fill_manual(values=c("purple","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=CV), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold"),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))+
  labs(title="",x="Year", y="Gini Coefficient\n(Higher = Less Dataset Diversity)", fill="PWC Category")+
  guides(color = "none")

gini.custom<-emmip(mlm.gini_2,CV+NLP+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
gini.custom<-gini.custom %>% filter(!(NLP==1)) %>% filter(!(CV==1))
gini.custom$Methods <- factor(gini.custom$methods, levels=c(1,0))
levels(gini.custom$Methods)<-c("Methods","No Domain Fixed Effects")
gini.custom.Methods <- ggplot(data=gini.custom, aes(x=date,y=yvar, color=Methods)) +
  scale_color_manual(values=c("red","green"))+
  scale_fill_manual(values=c("red","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=Methods), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=13),
        axis.title=element_text(size=15,face="bold"),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10))+
  labs(x="Year", title='',y='',fill="")+
  guides(color = "none")

#gini.full<-grid.arrange(gini.custom.CV,gini.custom.NLP,gini.custom.Methods,nrow=1)

#cdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsOnly.txt')
cdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsOnly.txt')
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
cdata<-cdata %>% filter(year<2021) %>% filter(year>2014)

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
  labs(y="Gini Coefficient\n(Higher = Less Dataset Diversity)",x="Year")+
  guides(color = "none",alpha='none',fill='none')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))

gini.all<-grid.arrange(gini.custom.CV,gini.custom.NLP,gini.custom.Methods,gini.full,nrow=2,
                        layout_matrix = rbind(c(1, 2, 3),
                                              c(4)))


ggsave('./Figures/GiniTrends.svg',gini.all,width=3246,height=2475,units='px')
ggsave('./Figures/GiniTrends.png',gini.all,width=3246,height=2475,units='px')


fdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsOnly.AllYears.txt')
#fdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsandChildren.AllYears.txt')

gini.aggregated<-ggplot(data=fdata,aes(x=factor(0),y=gini))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(CV==1),aes(x=factor(1),y=gini),fill='purple',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(CV==1),aes(x=factor(1),y=gini),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(CV==1),aes(x=factor(1),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=gini),fill='orange',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=gini),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=gini),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=gini,fill='red',alpha=.4))+
  geom_boxplot(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=gini),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=gini),
               fun=median, geom="point", size=2, color="red") +
  scale_x_discrete(labels= c("Full Dataset","Computer Vision","Natural Language Processing","Methods"))+
  labs(y="Gini Coefficient\n(Higher = Less Dataset Diversity)",x="")+
  guides(color = "none",alpha='none',fill='none')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))

gini.aggregated
 ggsave('./Figures/GiniRatios.svg',gini.aggregated,width=15.1,height=11.5,units='in')
ggsave('./Figures/GiniRatios.png',gini.aggregated,width=15.1,height=11.5,units='in')


###PIELOU EVENNESS SENSITIVITY ANALYSIS

mlm.pielou_0_full<-glmmTMB(gini_n2 ~ date*task_size*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.pielou_1<-glmmTMB(pielou_n2 ~ date*task_size+date*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.pielou_2<-glmmTMB(pielou_n2 ~ date*task_size+date+task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.pielou_3<-glmmTMB(pielou_n2 ~ date+task_size+date*task_age+CV*date+NLP*date+methods*date+(1|clusters), data=cdata,family=beta_family(link = "logit"))
mlm.pielou_4<-glmmTMB(pielou_n2 ~ date+task_size+date+task_age+CV*date+NLP*date+methods*date, data=cdata,family=beta_family(link = "logit"))
compare_performance(mlm.pielou_0_full, mlm.pielou_1, mlm.pielou_2,mlm.pielou_3,mlm.pielou_4)

plot(simulateResiduals(mlm.pielou_0_full,refit=F))
plot(simulateResiduals(mlm.pielou_n2_full,refit=F))

mylist <- list(task_size = quantile(cdata$task_size,p=c(.1,.25,.5,.75,.9)),date=1:(max(cdata$year)-min(cdata$year)+1),task_age=1:(max(cdata$year)-min(cdata$year)+1))
#mlm.pielou_0_full<-mlm.pielou_n2_full
pielou.custom<-emmip(mlm.pielou_2,NLP+CV+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
pielou.custom<-pielou.custom %>% filter(!(CV==1)) %>% filter(!(methods==1))
pielou.custom$NLP <- factor(pielou.custom$NLP, levels=c(1,0))
levels(pielou.custom$NLP)<-c("Natural Language Processing","No Domain Fixed Effects")
pielou.custom.NLP <- ggplot(data=pielou.custom, aes(x=date,y=yvar, color=NLP)) +
  scale_color_manual(values=c("orange","green"))+
  scale_fill_manual(values=c("orange","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=NLP), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position = c(0, 0), 
        legend.justification = c(0, 0),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))+
  labs(title="",x="Year",y='',fill="PWC\nCategory")+
  guides(color = "none")

pielou.custom<-emmip(mlm.pielou_2,CV+NLP+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
pielou.custom<-pielou.custom %>% filter(!(NLP==1)) %>% filter(!(methods==1))
pielou.custom$CV <- factor(pielou.custom$CV, levels=c(1,0))
levels(pielou.custom$CV)<-c("Computer Vision","No Domain Fixed Effects")
pielou.custom.CV <- ggplot(data=pielou.custom, aes(x=date,y=yvar, color=CV)) +
  scale_color_manual(values=c("purple","green"))+
  scale_fill_manual(values=c("purple","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=CV), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position = c(0, 0), 
        legend.justification = c(0, 0),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))+
  labs(title="",x="Year", y="Pielou Evenness\n(Higher = Less Dataset Diversity)", fill="PWC\nCategory")+
  guides(color = "none")

pielou.custom<-emmip(mlm.pielou_2,CV+NLP+methods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
pielou.custom<-pielou.custom %>% filter(!(NLP==1)) %>% filter(!(CV==1))
pielou.custom$Methods <- factor(pielou.custom$methods, levels=c(1,0))
levels(pielou.custom$Methods)<-c("Methods","No Domain FEs")
pielou.custom.Methods <- ggplot(data=pielou.custom, aes(x=date,y=yvar, color=Methods)) +
  scale_color_manual(values=c("red","green"))+
  scale_fill_manual(values=c("red","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=Methods), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position = c(0, 0), 
        legend.justification = c(0, 0),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))+
  labs(x="Year", title='',y='',fill="PWC\nCategory")+
  guides(color = "none")

pielou.full<-grid.arrange(pielou.custom.CV,pielou.custom.NLP,pielou.custom.Methods,nrow=1)
ggsave('./Figures/pielouTrends.svg',pielou.full)
ggsave('./Figures/pielouTrends.png',pielou.full)


fdata<-read_tsv('./Dataset_Curation/EntropyDatasetforRParentsOnly.AllYears.txt')

pielou.aggregated<-ggplot(data=fdata,aes(x=factor(0),y=pielou))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(CV==1),aes(x=factor(1),y=pielou),fill='purple',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(CV==1),aes(x=factor(1),y=pielou),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(CV==1),aes(x=factor(1),y=pielou),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=pielou),fill='orange',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=pielou),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=pielou),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=pielou,fill='red',alpha=.4))+
  geom_boxplot(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=pielou),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(Methodology==1),aes(x=factor(3),y=pielou),
               fun=median, geom="point", size=2, color="red") +
  scale_x_discrete(labels= c("Full Dataset","Computer Vision","Natural Language Processing","Methods"))+
  labs(y="pielou Coefficient\n(Higher = Less Dataset Diversity)",x="")+
  guides(color = "none",alpha='none',fill='none')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1),
        legend.title = element_text(size=18),
        legend.text = element_text(size=16))

pielou.aggregated
ggsave('./Figures/pielouRatios.svg',pielou.aggregated,width=15.1,height=11.5,units='in')
ggsave('./Figures/pielouRatios.png',gini.aggregated,width=15.1,height=11.5,units='in')






















emmip(mlm.gini0I,year~, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Higher = More Concentrated")+
  #  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Gini Inequality")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

gini_residuals <- simulateResiduals(mlm.gini)
testZeroInflation(gini_residuals)
plot(gini_residuals)
plot(allEffects(mlm.gini))
gini.tests <- dredge(mlm.gini)

mlm.simpson0<-glmmTMB(1-simpson ~ year+task_size*year+task_age*task_size+(1|clusters)+(1|task_age),ziformula=~1+size,data=cdata,family=beta_family(link = "logit"))
summary(mlm.simpson)
simpson.full<-emmip(mlm.simpson0,task_size ~ task_age, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Higher = More Concentrated")+
  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Gini Inequality")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

###IMAGES###
mylist <- list(task_size = quantile(idata$task_size,p=c(.25,.5,.75)),date=1:7,task_age=1:7)
real_quantiles=quantile(idata$size,p=c(.25,.5,.75))

mlm.pielou.images<-glmmTMB(pielou_n ~ date+task_size*date+task_age*task_size+(1|clusters),data=idata,family=beta_family())
summary(mlm.pielou.images)
pielou.images<-emmip(mlm.pielou.images,task_size ~ date, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Lower = Concentrated")+
  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Images")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

mlm.gini.images<-glmmTMB(gini_n ~ date+task_size*date+task_age*task_size+(1|clusters),data=idata,family=beta_family())
summary(mlm.gini.images)
gini.images<-emmip(mlm.gini.images,task_size ~ date, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Higher = Concentrated")+
  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Images")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

##TEXTS####
mylist <- list(task_size = quantile(tdata$task_size,p=c(.25,.5,.75)),date=1:7,task_age=1:7)
real_quantiles=quantile(tdata$size,p=c(.25,.5,.75))

mlm.pielou.texts<-glmmTMB(pielou_n ~ date+task_size*date+task_age*task_size+(1|clusters),data=tdata,family=beta_family())
summary(mlm.pielou.texts)
pielou.texts<-emmip(mlm.pielou.texts,task_size ~ date, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Lower = Concentrated")+
  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Texts")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

mlm.gini.texts<-glmmTMB(gini_n ~ date+task_size*date+task_age*task_size+(1|clusters),data=tdata,family=beta_family())
summary(mlm.gini.texts)
gini.texts<-emmip(mlm.gini.texts,task_size ~ date, at = mylist,CIs = T, type = 'response')+
  scale_x_continuous(labels=function(x)x+2013,n.breaks=7)+
  scale_y_continuous(labels=function(x)round(x,2),name="Higher = Concentrated")+
  scale_color_discrete(labels=paste0(as.character(real_quantiles),' (',names(real_quantiles),')'),name="Task Size") +
  ggtitle("Texts")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))
# model.sel(creation_ratio_full,creation_ratio_full0,creation_ratio_1,creation_ratio_2,creation_ratio_3,
#           creation_ratio_4,creation_ratio_5,creation_ratio_6,creation_ratio_7)
entropy.analyses<-grid.arrange(pielou.full,gini.full,
                             pielou.images,gini.images,
                             pielou.texts,gini.texts,
                             ncol=2,nrow=3)
