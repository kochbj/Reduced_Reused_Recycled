library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(emmeans)
library(gridExtra)
library(performance)

setwd("C:/Users/berna/Documents/Github/Life_of_a_Benchmark")

#### LOADING AND FORMATTING DATA ####
#NOTE THAT THESE DATASETS ARE CREATED BY THE MainDataset.ipynb FILE.

#This is the data for each task aggregated across all years.
#See the section of the dataset curation file for RQ2 to see how this is created.
#fdata<-read_tsv('./Dataset_Curation/PWC_Data/Derivative_Datasets/FullDatasetforR.ParentsandChildren.AllYears.txt')
fdata<-read_tsv('./Dataset_Curation/PWC_Data/Derivative_Datasets/FullDatasetforR.ParentsOnly.AllYears.txt')

fdata$CV<-as.factor(fdata$CV)
fdata$NLP<-as.factor(fdata$NLP)
fdata$Methods<-as.factor(fdata$Methodology)

#This is the data for each task disaggregated across all years.
cdata<-read_tsv('./Dataset_Curation/PWC_Data/Derivative_Datasets/FullDatasetforR.ParentsOnly.txt')
#If you'd like to look at this file as a sensitivity analysis, you should be able to run it and run the code.
#cdata<-read_tsv('./Dataset_Curation/PWC_Data/Derivative_Datasets/FullDatasetforR.ParentsOnly.AllYears.NotMedians.txt')

#Note that most of this is just massaging data to improve convergence of regression models.
#I note two places where data is really filtered.
cdata$creation_ratio<-as.numeric(cdata$creation_ratio) 
cdata$clusters<-as.factor(cdata$task)
#Here's where we filter by time
cdata<-cdata %>% filter(year<2021) %>% filter(year>2014)
cdata$date<-cdata$year-min(cdata$year)
#Remove task-years with fewer than 10 usages
cdata<-cdata %>% filter(size>=10) 
cdata$task_age<-scale(cdata$task_age,center=T,scale=F)
cdata$full_size<-scale(cdata$pwc_size,center=T,scale=T)
cdata$task_size<-scale(cdata$size,center=T,scale=F)
cdata$CV<-as.factor(cdata$CV)
cdata$NLP<-as.factor(cdata$NLP)
cdata$Methods<-as.factor(cdata$Methodology)

# This is not used in the main model,
#but I discuss in the appendix that I was worried that growth in PWC could be confounding
#even though we already control for task age and task size.
#It doesn't make a difference in the regression models.
pwc_size_resid<-resid(lm(log(pwc_size)~date,distinct(cdata[c("date","pwc_size")])))
pwc_size_resid<-cbind(pwc_size_resid,distinct(cdata['date']))
cdata<-merge(cdata,pwc_size_resid,by='date')


#### REGRESSION MODELS ####
#I didnt report these because I just wasn't comfortable with how sparse/noisy data was, but you can look at them.
adopRatio<-cbind(cdata$num_papers_adopting,cdata$num_papers_growing+cdata$num_papers_adopting)
mlm.adop_0_full<-glmmTMB(adopRatio ~ date*task_size*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.adop_1<-glmmTMB(adopRatio ~ date*task_size+date*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.adop_2<-glmmTMB(adopRatio ~ date*task_size+date+task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.adop_3<-glmmTMB(adopRatio ~ date+task_size+date*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.adop_4<-glmmTMB(adopRatio ~ date+task_size+date+task_age+CV*date+NLP*date+Methods*date, data=cdata,family='binomial')
compare_performance(mlm.adop_0_full, mlm.adop_1, mlm.adop_2,mlm.adop_3,mlm.adop_4)


creatRatio<-cbind(cdata$num_dataset_births,cdata$num_dataset_imports+cdata$num_dataset_births)
mlm.creat_0_full<-glmmTMB(creatRatio ~ date*task_size*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.creat_1<-glmmTMB(creatRatio ~ date*task_size+date*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.creat_2<-glmmTMB(creatRatio ~ date*task_size+date+task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.creat_3<-glmmTMB(creatRatio ~ date+task_size+date*task_age+CV*date+NLP*date+Methods*date+(1|clusters), data=cdata,family='binomial')
mlm.creat_4<-glmmTMB(creatRatio ~ date+task_size+date+task_age+CV*date+NLP*date+Methods*date, data=cdata,family='binomial')
compare_performance(mlm.creat_0_full, mlm.creat_1, mlm.creat_2,mlm.creat_3,mlm.creat_4)


#### CONSTRUCTING FIGURE 2####

plot(density(fdata$adoption_pct,na.rm=T))
adoption.aggregated<-ggplot(data=fdata,aes(x=factor(0),y=adoption_pct))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(CV==1),aes(x=factor(1),y=adoption_pct),fill='purple',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(CV==1),aes(x=factor(1),y=adoption_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(CV==1),aes(x=factor(1),y=adoption_pct),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=adoption_pct),fill='orange',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=adoption_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=adoption_pct),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=adoption_pct,fill='red',alpha=.4))+
  geom_boxplot(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=adoption_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=adoption_pct),
               fun=median, geom="point", size=2, color="red") +
  scale_x_discrete(labels= c("Full Dataset","Computer Vision","Natural Language Processing","Methods"))+
  labs(y="Adoption Proportion\n(% Papers Using Imported Datasets)",x="")+
  guides(color = "none",alpha='none',fill='none')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=18,face="bold"),
        axis.title=element_text(size=18,face="bold"))


creation.aggregated<-ggplot(data=fdata,aes(x=factor(0),y=creation_pct))+
  geom_violin(fill='green',alpha=.4)+
  geom_boxplot(width=0.1,alpha=.5)+
  stat_summary(fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(CV==1),aes(x=factor(1),y=creation_pct),fill='purple',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(CV==1),aes(x=factor(1),y=creation_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(CV==1),aes(x=factor(1),y=creation_pct),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=creation_pct),fill='orange',alpha=.4)+
  geom_boxplot(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=creation_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(NLP==1),aes(x=factor(2),y=creation_pct),
               fun=median, geom="point", size=2, color="red")+
  geom_violin(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=creation_pct,fill='red',alpha=.4))+
  geom_boxplot(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=creation_pct),width=.1,alpha=.5)+
  stat_summary(data=fdata %>% filter(Methods==1),aes(x=factor(3),y=creation_pct),
               fun=median, geom="point", size=2, color="red") +
  scale_x_discrete(labels= c("Full Dataset","Computer Vision","Natural Language Processing","Methods"))+
  labs(y="Creation Proportion\n(% Papers Using Imported Datasets)",x="")+
  guides(color = "none",alpha='none',fill='none')+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=18,face="bold"),
        axis.title=element_text(size=18,face="bold"))

ratios_final<-grid.arrange(adoption.aggregated,creation.aggregated)
ggsave('./Figures/Figure2.svg',ratios_final,width=15.1,height=11.5,units='in')
ggsave('./Figures/Figure2.png',ratios_final,width=15.1,height=11.5,units='in')


#### CORRELATIONS ON LINE 289-291 ####

ggplot(data=fdata,aes(x=size,y=creation_pct))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous(trans='log2')+
  scale_y_continuous(trans='log2') 

ggplot(data=fdata,aes(x=size,y=adoption_pct))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous(trans='log2')+
  scale_y_continuous(trans='log2') 

cor.test(fdata$size,fdata$creation_pct,method = 'kendall')
cor.test(fdata %>% filter(NLP==1) %>% pull(size),fdata %>% filter(NLP==1) %>% pull(creation_pct) ,method = 'kendall')


cor.test(fdata$size,fdata$adoption_pct,method = 'kendall')
cor.test(fdata %>% filter(NLP==1) %>% pull(size),fdata %>% filter(NLP==1) %>% pull(adoption_pct) ,method = 'kendall')
