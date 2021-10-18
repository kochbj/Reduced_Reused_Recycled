library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(emmeans)
library(gridExtra)


fdata<-read_tsv('./GoogleDataProject/RatioInputs/FullDatasetforR.ParentsOnly.AllYears.txt')
fdata$CV<-as.factor(fdata$CV)
fdata$NLP<-as.factor(fdata$NLP)
fdata$cvmethods<-as.factor(fdata$cvmethods)
fdata$Methods<-as.factor(fdata$cvmethods)

cdata<-read_tsv('./GoogleDataProject/RatioInputs/FullDatasetforR.ParentsOnly.txt')
cdata$creation_ratio<-as.numeric(cdata$creation_ratio)
cdata$clusters<-as.factor(cdata$task)
cdata<-cdata %>% filter(year<2021) %>% filter(year>2010)
cdata$date<-cdata$year-min(cdata$year)
cdata<-cdata %>% filter(size>=5) 
cdata$task_age<-scale(cdata$task_age,center=T,scale=F)
cdata$full_size<-scale(cdata$pwc_size,center=T,scale=T)
cdata$task_size<-scale(cdata$size,center=T,scale=F)
cdata$CV<-as.factor(cdata$CV)
cdata$NLP<-as.factor(cdata$NLP)
cdata$Methods<-as.factor(cdata$Methodology)


# 
# creation_ratio_full<-glmmTMB(creatRatio ~ full_size+date*task_size*task_age+CV*date+NLP*date+cvmethods*date+(1|clusters),data=cdata,family='binomial')
# creation_ratio_1<-glmmTMB(creatRatio ~ date+task_size+task_age,data=cdata,family='binomial')
# creation_ratio_2<-glmmTMB(creatRatio ~ date*task_size*task_age,data=cdata,family='binomial')
# creation_ratio_3<-glmmTMB(creatRatio ~ full_size+date*task_size*task_age,data=cdata,family='binomial')
# creation_ratio_4<-glmmTMB(creatRatio ~ full_size+date+task_size+task_age,data=cdata,family='binomial')
# creation_ratio_5<-glmmTMB(creatRatio ~ full_size+date+task_size,data=cdata,family='binomial')
# creation_ratio_6<-glmmTMB(creatRatio ~ date+task_size,data=cdata,family='binomial')
# creation_ratio_7<-glmmTMB(creatRatio ~ date+full_size,data=cdata,family='binomial')
# 
# model.sel(creation_ratio_full,creation_ratio_full0,creation_ratio_1,creation_ratio_2,creation_ratio_3,
#           creation_ratio_4,creation_ratio_5,creation_ratio_6,creation_ratio_7)
# 




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
  labs(y="Adoption Ratio\n(% Papers Using Imported Datasets)",x="")+
  guides(color = "none",alpha='none',fill='none')

ggplot(data=fdata,aes(x=size,y=adoption_pct))+
geom_point()+
geom_smooth()+
scale_x_continuous(trans='log2')+
scale_y_continuous(trans='log2') 
cor.test(fdata$size,fdata$adoption_pct,method = 'spearman')

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
  labs(y="Creation Ratio\n(% Papers Using Imported Datasets)",x="")+
  guides(color = "none",alpha='none',fill='none')

ggplot(data=fdata,aes(x=size,y=creation_pct))+
  geom_point()+
  geom_smooth()+
  scale_x_continuous(trans='log2')+
  scale_y_continuous(trans='log2') 
cor.test(fdata$size,fdata$creation_pct,method = 'spearman')


ratios_final<-grid.arrange(adoption.aggregated,creation.aggregated)
ggsave('./GoogleDataProject/RatioInputs/ViolinRatios.svg',ratios_final)
ggsave('./GoogleDataProject/RatioInputs/ViolinRatios.png',ratios_final)

plot(adoption.aggregated)

ratios.overtime <-cdata %>%  group_by(year) %>% summarise(adopt.50=median(adoption_pct,na.rm=T),
                                                  adopt.25=quantile(adoption_pct,.25,na.rm=T), 
                                                  adopt.75=quantile(adoption_pct,.75,na.rm=T),
                                                  creation.50=mean(creation_pct,na.rm=T),
                                                  creation.25=quantile(creation_pct,.25,na.rm=T), 
                                                  creation.75=quantile(creation_pct,.75,na.rm=T),
                                                  conversion.50=median(conversion_pct,na.rm=T),
                                                  conversion.25=quantile(conversion_pct,.25,na.rm=T), 
                                                  conversion.75=quantile(conversion_pct,.75,na.rm=T))
ratios.overtime.CV <-cdata %>% filter(CV==1) %>% group_by(year) %>% summarise(adopt.50=median(adoption_pct,na.rm=T),
                                                                                  adopt.25=quantile(adoption_pct,.25,na.rm=T), 
                                                                                  adopt.75=quantile(adoption_pct,.75,na.rm=T),
                                                                                  creation.50=mean(creation_pct,na.rm=T),
                                                                                  creation.25=quantile(creation_pct,.25,na.rm=T), 
                                                                                  creation.75=quantile(creation_pct,.75,na.rm=T),
                                                                                  conversion.50=median(conversion_pct,na.rm=T),
                                                                                  conversion.25=quantile(conversion_pct,.25,na.rm=T), 
                                                                                  conversion.75=quantile(conversion_pct,.75,na.rm=T))

ratios.overtime.NLP <-cdata %>% filter(NLP==1) %>% group_by(year) %>% summarise(adopt.50=median(adoption_pct,na.rm=T),
                                                                                            adopt.25=quantile(adoption_pct,.25,na.rm=T), 
                                                                                            adopt.75=quantile(adoption_pct,.75,na.rm=T),
                                                                                            creation.50=mean(creation_pct,na.rm=T),
                                                                                            creation.25=quantile(creation_pct,.25,na.rm=T), 
                                                                                            creation.75=quantile(creation_pct,.75,na.rm=T),
                                                                                            conversion.50=median(conversion_pct,na.rm=T),
                                                                                            conversion.25=quantile(conversion_pct,.25,na.rm=T), 
                                                                                            conversion.75=quantile(conversion_pct,.75,na.rm=T))

ratios.overtime.Methods <-cdata %>% filter(Methodology==1) %>% group_by(year) %>% summarise(adopt.50=median(adoption_pct,na.rm=T),
                                                                                               adopt.25=quantile(adoption_pct,.25,na.rm=T), 
                                                                                               adopt.75=quantile(adoption_pct,.75,na.rm=T),
                                                                                               creation.50=mean(creation_pct,na.rm=T),
                                                                                               creation.25=quantile(creation_pct,.25,na.rm=T), 
                                                                                               creation.75=quantile(creation_pct,.75,na.rm=T),
                                                                                               conversion.50=median(conversion_pct,na.rm=T),
                                                                                               conversion.25=quantile(conversion_pct,.25,na.rm=T), 
                                                                                               conversion.75=quantile(conversion_pct,.75,na.rm=T))


ratios.overtime.plot<- ggplot(ratios.overtime, aes(year, adopt.50)) +
  geom_line(color='green',size=2) +
  geom_ribbon(aes(ymin = adopt.25, ymax = adopt.75), alpha = .1,fill='green')+
  scale_y_continuous(name="Gini Coefficient (Higher = Less Diverse)") +
  scale_x_continuous(name="Year")+
  guides(color = "none")+
  geom_line(data=ratios.overtime.CV,aes(x=year,y=adopt.50,color='purple',size=2))+
  geom_line(data=ratios.overtime.NLP,aes(x=year,y=adopt.50,color='orange',size=2))+
  geom_line(data=ratios.overtime.Methods,aes(x=year,y=adopt.50,color='red',size=2))+
  ggtitle("Diversity of Dataset-Introducing Institutions Over Time")+theme(plot.title = element_text(hjust = 0.5))









mylist <- list(task_size = quantile(cdata$task_size,p=c(.5)),date=1:(max(cdata$year)-min(cdata$year)+1),task_age=1:(max(cdata$year)-min(cdata$year)+1))
creatRatio<-cbind(cdata$num_dataset_births,cdata$num_dataset_imports+cdata$num_dataset_births)

creation_ratio_full<-glmmTMB(creatRatio ~ full_size+date*task_size*task_age+CV*date+NLP*date+cvmethods*date,data=cdata,family='binomial')
plot(simulateResiduals(creation_ratio_full,refit=F))

emmip(creation_ratio_full,NLP ~ date, at = mylist,CIs = T, type = 'response',plotit=T)

#creation_fits<-dredge(creation_ratio_full,extra = c(AIC,BIC,Cp),fixed=c("cond(date)"))
#AFTER ALL THIS FITTING THERE IS INDICATION FOR THE FULL MODEL

real_quantiles=quantile(cdata$size,p=c(.25,.5,.75))

creation.custom<-emmip(creation_ratio_full,NLP+CV+cvmethods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
creation.custom<-creation.custom %>% filter(!(CV==1)) %>% filter(!(cvmethods==T)) 
creation.custom$NLP <- factor(creation.custom$NLP, levels=c(1,0))
levels(creation.custom$NLP)<-c("NLP","Marginal")
creation.custom$NLP %>% filter(NLP!="Marginal")
creation.custom.NLP <- ggplot(data=creation.custom, aes(x=date,y=yvar)) +
  scale_color_manual(values=c("orange","green"))+
  scale_fill_manual(values=c("orange","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=NLP), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Creation Ratio by Category (NLP)",x="Year", y="Creation Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

creation.custom<-emmip(creation_ratio_full,CV+NLP+cvmethods ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
creation.custom<-creation.custom %>% filter(!(NLP==1)) %>% filter(!(cvmethods==T))
creation.custom$CV <- factor(creation.custom$CV, levels=c(1,0))
levels(creation.custom$CV)<-c("CV","Marginal")
creation.custom<-creation.custom %>% filter(CV!='Marginal')
creation.custom.CV <- ggplot(data=creation.custom, aes(x=date,y=yvar, color=CV)) +
  scale_color_manual(values=c("purple","green"))+
  scale_fill_manual(values=c("purple","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=CV), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Creation Ratio by Category (CV)",x="Year", y="Creation Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

creation.custom<-emmip(creation_ratio_full,CV+NLP+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
creation.custom<-creation.custom %>% filter(!(NLP==1)) %>% filter(!(CV==1))
creation.custom$Methods <- factor(creation.custom$Methodology, levels=c(T,F))
levels(creation.custom$Methods)<-c("Methods","Marginal")
creation.custom.Methods <- ggplot(data=creation.custom, aes(x=date,y=yvar, color=Methods)) +
  scale_color_manual(values=c("red","green"))+
  scale_fill_manual(values=c("red","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=Methods), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Diversity of Tasks by Category (Methods)",x="Year", y="Creation Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")


adopRatio<-cbind(cdata$num_papers_adopting,cdata$num_papers_growing+cdata$num_papers_adopting)
adoption_ratio_full<-glmmTMB(adopRatio ~ full_size+date*task_size*task_age+CV*date+NLP*date+Methodology*date+(1|clusters),data=cdata,family='binomial')
mylist <- list(task_size = quantile(cdata$task_size,p=c(.5)),date=1:(max(cdata$year)-min(cdata$year)+1),task_age=1:(max(cdata$year)-min(cdata$year)+1))

adoption.custom<-emmip(adoption_ratio_full,NLP+CV+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
adoption.custom<-adoption.custom %>% filter(!(CV==1)) %>% filter(!(Methodology==T))
adoption.custom$NLP <- factor(adoption.custom$NLP, levels=c(1,0))
levels(adoption.custom$NLP)<-c("NLP","Marginal")
adoption.custom.NLP <- ggplot(data=adoption.custom, aes(x=date,y=yvar, color=NLP)) +
  scale_color_manual(values=c("orange","green"))+
  scale_fill_manual(values=c("orange","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=NLP), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  #scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Adoption Ratio by Category (NLP)",x="Year", y="Adoption Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

adoption.custom<-emmip(adoption_ratio_full,CV+NLP+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
adoption.custom<-adoption.custom %>% filter(!(NLP==1)) %>% filter(!(Methodology==T))
adoption.custom$CV <- factor(adoption.custom$CV, levels=c(1,0))
levels(adoption.custom$CV)<-c("CV","Marginal")
adoption.custom.CV <- ggplot(data=adoption.custom, aes(x=date,y=yvar, color=CV)) +
  scale_color_manual(values=c("purple","green"))+
  scale_fill_manual(values=c("purple","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=CV), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  #scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Diversity of Tasks by Category (CV)",x="Year", y="Adoption Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

adoption.custom<-emmip(adoption_ratio_full,CV+NLP+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
adoption.custom<-adoption.custom %>% filter(!(NLP==1)) %>% filter(!(CV==1))
adoption.custom$Methods <- factor(adoption.custom$Methodology, levels=c(T,F))
levels(adoption.custom$Methods)<-c("Methods","Marginal")
adoption.custom.Methods <- ggplot(data=adoption.custom, aes(x=date,y=yvar, color=Methods)) +
  scale_color_manual(values=c("red","green"))+
  scale_fill_manual(values=c("red","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=Methods), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  #scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Diversity of Tasks by Category (Methods)",x="Year", y="Adoption Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")


conRatio<-cbind(cdata$num_converted_growing,cdata$num_papers_growing)
conversion_ratio_full<-glmmTMB( conRatio ~ full_size+date*task_size*task_age+CV*date+NLP*date+Methodology*date+(1|clusters),data=cdata,family='binomial')
#conversion_fits<-dredge(conversion_ratio_full,extra = c(AIC,BIC,Cp),fixed=c("cond(date)"))

conversion.custom<-emmip(conversion_ratio_full,NLP+CV+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
conversion.custom<-conversion.custom %>% filter(!(CV==1)) %>% filter(!(Methodology==T))
conversion.custom$NLP <- factor(conversion.custom$NLP, levels=c(1,0))
levels(conversion.custom$NLP)<-c("NLP","Marginal")
conversion.custom.NLP <- ggplot(data=conversion.custom, aes(x=date,y=yvar, color=NLP)) +
  scale_color_manual(values=c("orange","green"))+
  scale_fill_manual(values=c("orange","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=NLP), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="conversion Ratio by Category (NLP)",x="Year", y="conversion Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

conversion.custom<-emmip(conversion_ratio_full,CV+NLP+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
conversion.custom<-conversion.custom %>% filter(!(NLP==1)) %>% filter(!(Methodology==T))
conversion.custom$CV <- factor(conversion.custom$CV, levels=c(1,0))
levels(conversion.custom$CV)<-c("CV","Marginal")
conversion.custom.CV <- ggplot(data=conversion.custom, aes(x=date,y=yvar, color=CV)) +
  scale_color_manual(values=c("purple","green"))+
  scale_fill_manual(values=c("purple","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=CV), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Diversity of Tasks by Category (CV)",x="Year", y="conversion Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

conversion.custom<-emmip(conversion_ratio_full,CV+NLP+Methodology ~ date, at = mylist,CIs = T, type = 'response',plotit=F)
conversion.custom<-conversion.custom %>% filter(!(NLP==1)) %>% filter(!(CV==1))
conversion.custom$Methods <- factor(conversion.custom$Methodology, levels=c(T,F))
levels(conversion.custom$Methods)<-c("Methods","Marginal")
conversion.custom.Methods <- ggplot(data=conversion.custom, aes(x=date,y=yvar, color=Methods)) +
  scale_color_manual(values=c("red","green"))+
  scale_fill_manual(values=c("red","green"))+
  geom_line(size=1.5)+
  geom_ribbon(aes(ymax=UCL, ymin=LCL, fill=Methods), alpha=0.4)+
  scale_x_continuous(labels=function(x)x+min(cdata$year)-1,n.breaks=10)+
  scale_y_continuous(limits=c(0,1),n.breaks=8)+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(1, 1), 
        legend.justification = c(1, 1))+
  labs(title="Diversity of Tasks by Category (Methods)",x="Year", y="conversion Ratio\n(Higher= More Task-specific Datasets Adopted)", fill="PWC\nCategory")+
  guides(color = "none")

ratio.analyses<-grid.arrange(adoption.custom.CV,creation.custom.CV,conversion.custom.CV,
                            adoption.custom.NLP,creation.custom.NLP,conversion.custom.NLP,
                            adoption.custom.Methods,creation.custom.Methods,conversion.custom.Methods,
                            ncol=3,nrow=3)


emmip(conversion_ratio_full,CV ~ date, at = mylist,CIs = T, type = 'response',plotit=T)