# Microdosing Experiences
# Vince Polito
# vince.polito@mq.edu.au
# 
# This script relies on csv files in the cleandata subfolder
# This script performs LME analysis and generates plots for longterm data in Study1.
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(nlme,ggpubr,ggthemes, psych,tidyverse)
p_load_gh("vince-p/vtools")

pre.post<-read.csv("cleandata/longterm.csv")

# Setup contrasts - I'm not sure exactly what these lines do.
contrasts(pre.post$Time)=contr.sum(2)
contrasts(pre.post$experience)=contr.sum(2)

############
## working code to try out a single analysis
DASS_Depression.r = lme(DASS_Depression~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=pre.post) #Test only main effects and the two specific 2-way interactions of interest
t<-summary(DASS_Depression.r)
#EXTRACT AND ADJUST P-VALUES
pvals=summary(DASS_Depression.r)$tTable[c('Time1', 'Time1:experience1', 'Time1:maxdoses.c'),'p-value']
p.adjust(pvals, method = "holm")
pv(pvals,"holm")
############

.d=pre.post[5:20] # convenience subset of numeric totals data
.t<-describeBy(.d,pre.post$Time) # generate summary table with descripties of pre.post variables
pp.descriptives<-data.frame(var=names(.d),BaseMean=.t$base$mean,BaseSD=.t$base$sd,PostMean=.t$post$mean,PostSD=.t$post$sd) #save relevant stats in a new df

#code from https://stackoverflow.com/questions/26357429/how-to-use-substitute-to-loop-lme-functions-from-nlme-package
#generate uncorrected model (for table S2)
pp.uncorrected<- lapply(names(pre.post[5:20]), function(k) {
  temp.summary<-summary(lme(eval(substitute(q ~ Time*experience+Time*maxdoses.c, list(q = as.name(k)))), random = ~1|id, method="ML",data = pre.post,na.action=na.exclude),digits=3)
   temp.uncorrected<-data.frame(k,Intercept=as.numeric(temp.summary$tTable["(Intercept)",c("Value")]),
                            var="Time",b=as.numeric(temp.summary$tTable["Time1",c("Value")]),
                            T=as.numeric(temp.summary$tTable["Time1",c("t-value")]),
                            P=pv(temp.summary$tTable["Time1",c("p-value")]),
                            var4="Experience",b4=temp.summary$tTable["experience1",c("Value")],
                            T4=temp.summary$tTable["experience1",c("t-value")],
                            P4=pv(temp.summary$tTable["experience1",c("p-value")]),
                            var5="Doses",b4=temp.summary$tTable["maxdoses.c",c("Value")],
                            T5=temp.summary$tTable["maxdoses.c",c("t-value")],
                            P5=pv(temp.summary$tTable["maxdoses.c",c("p-value")]),
                            var2="TimeXExperience",b2=temp.summary$tTable["Time1:experience1",c("Value")],
                            T2=temp.summary$tTable["Time1:experience1",c("t-value")],
                            P2=pv(temp.summary$tTable["Time1:experience1",c("p-value")]),
                            var3="TimeXDoses",b3=temp.summary$tTable["Time1:maxdoses.c",c("Value")],
                            T3=temp.summary$tTable["Time1:maxdoses.c",c("t-value")],
                            P3=pv(temp.summary$tTable["Time1:maxdoses.c",c("p-value")])
                            )
   
   }) 

#generate corrected table for manuscript table 4
pp.corrected<- lapply(names(pre.post[5:20]), function(k) {
  temp.summary<-summary(lme(eval(substitute(q ~ Time*experience+Time*maxdoses.c, list(q = as.name(k)))), random = ~1|id, method="ML",data = pre.post,na.action=na.exclude),digits=3)
  temp.corrected<-data.frame(k,Intercept=as.numeric(temp.summary$tTable["(Intercept)",c("Value")]),
             var="Time",b=as.numeric(temp.summary$tTable["Time1",c("Value")]),
             T=as.numeric(temp.summary$tTable["Time1",c("t-value")]),
             P=pv(temp.summary$tTable["Time1",c("p-value")],"holm",3),
              var2="TimeXExperience",b2=temp.summary$tTable["Time1:experience1",c("Value")],
             T2=temp.summary$tTable["Time1:experience1",c("t-value")],
             P2=pv(temp.summary$tTable["Time1:experience1",c("p-value")],"holm",3),
              var3="TimeXDoses",b3=temp.summary$tTable["Time1:maxdoses.c",c("Value")],
             T3=temp.summary$tTable["Time1:maxdoses.c",c("t-value")],
            P3=pv(temp.summary$tTable["Time1:maxdoses.c",c("p-value")],"holm",3))
}) 

pp.uncorrected<-do.call(rbind,pp.uncorrected)
pp.corrected<-do.call(rbind,pp.corrected) # this mysterious line transforms lapply output into a useable df. Is there a better way to do this?

round_df(pp.descriptives,1)
round_df(pp.uncorrected,2)
round_df(pp.corrected,2)



# code to generate plots

#####
#Code for the example legend plot
set.seed(100)
dat = data.frame(y=rf(50, 5, 15), x="none")

plt<-ggplot(data = dat, aes(y = y,
                            x = x,
                            fill = x)) +
  xlab("")+
  ylab("")+
  theme(text = element_text(size=20),
        #axis.text = element_text(angle=0, hjust=1),
        axis.line = element_blank(), panel.background = element_blank(),
        axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
#print(plt)
(legend=plt + geom_violin(alpha = 0.5,
                          color = "grey", fill="lightgrey") +
    geom_boxplot(notch = TRUE,
                 width = 0.2, fill="darkgrey")+
    geom_point(x=1, y=mean(dat$y), size=2, shape=4)+
    geom_text(label="boxplot", x=.8, y=2,size=5,fontface="plain")+geom_segment(x=.8, y=1.8, xend=.89, yend=1.3)+
    geom_text(label="violin plot", x=.8, y=4,size=5,fontface="plain")+geom_segment(x=.86, y=3.9, xend=.95, yend=3.1)+
    geom_text(label="median", x=1.1, y=0,size=5,fontface="plain")+geom_segment(x=1.1, y=0.1, xend=1, yend=median(dat$y)-.05)+
    geom_text(label="mean", x=1.2, y=2,size=5,fontface="plain")+geom_segment(x=1.2, y=1.8, xend=1.02, yend=mean(dat$y)+.05)+
    geom_text(label="extreme values", x=1.2, y=mean(sort(dat$y, dec=T)[1:2]),size=5,fontface="plain")+
    geom_segment(x=1.02, y=sort(dat$y, dec=T)[1], 
                 xend=1.14, yend=(mean(sort(dat$y, dec=T)[1:2]))+.2)+
    geom_segment(x=1.02, y=sort(dat$y, dec=T)[2], 
                 xend=1.14, yend=(mean(sort(dat$y, dec=T)[1:2]))-.2)+
    labs(title = "Legend") +
    guides(fill=FALSE) +
    theme_hc() +
    theme(plot.title = element_text(hjust = 0.5,size=19),
          plot.margin=unit(c(1,0,2,0), "lines"),
          #panel.border = element_rect(colour = "black", fill=NA, size=1),
    ) 
)
#####


#Code for the actual plots

prefixlist<-unique(data.table::tstrsplit(colnames(pre.post[5:19]), split="\\_")[[1]])

pre.post$Time = factor(pre.post$Time, levels=c("base", "post"))

names(pre.post)[5:20]<-c("DASS_Depression", "DASS_Anxiety", "DASS_Stress", "MWQ_Mean", "QOLI_Total", "MAAS_Total","HMS_Total", "M5P_Extraversion","M5P_Agreeableness", "M5P_Conscientiousness","M5P_Neuroticism", "M5P_Openness", "TAS_Absorption", "CPS_Total", "SOARS_Involunatriness","SOARS_Effortlessness")
#names(pre.post)[5:20]<-gsub("_"," ",names(pre.post)[5:20])

ppplots<-lapply(names(pre.post)[5:20],function(x){ #20
  ggplot(data = pre.post, aes_string(y = x, x = "Time", fill="Time")) +
    geom_violin(alpha = 0.5,
                 color = NA) +
    geom_boxplot(notch = TRUE,
                width = 0.2) +
    stat_summary(aes_string(y = x, group = 1), fun.y=mean, geom="line", color="green4", size=.6) +
    stat_summary(fun.y="mean", geom="point", size=2, shape=4)+
    labs(x=NULL,y=NULL,title = x) +
    guides(fill=FALSE) +
    theme(axis.text.x = element_text(face="bold",  size=15),
          axis.text.y = element_text(face="bold",  size=16),
          plot.title = element_text(hjust = 0.5,size=19),
          plot.margin=unit(c(1,0,2,0), "lines")) +
    theme_hc()
  })

#ppplots[1]

#This next part sets the y axis values to better show the data
ppplots[[1]]$coordinates$limits$y=c(0,20) #Dass Depression
ppplots[[2]]$coordinates$limits$y=c(0,20) #DASS Anxiety
ppplots[[3]]$coordinates$limits$y=c(0,20) #DASS Stress
ppplots[[4]]$coordinates$limits$y=c(1,6)  #MWQ
ppplots[[5]]$coordinates$limits$y=c(-4,6) #QOLI
ppplots[[6]]$coordinates$limits$y=c(0,6)  #MAAS
ppplots[[8]]$coordinates$limits$y=c(1.8,4.4) #Extraversion
ppplots[[9]]$coordinates$limits$y=c(1.8,4.4) #Agreableness
ppplots[[10]]$coordinates$limits$y=c(1.8,4.4)#Conscientiousness
ppplots[[11]]$coordinates$limits$y=c(1.8,4.4)#Neuroticism
ppplots[[12]]$coordinates$limits$y=c(1.8,4.4)#Openness
ppplots[[13]]$coordinates$limits$y=c(7,34) #TAS
ppplots[[15]]$coordinates$limits$y=c(10,25) #Involuntariness
ppplots[[16]]$coordinates$limits$y=c(10,25) #Effortlessness


# To make this work (using the "legend" box to fill in the blank bit), you have to use a nested
# ggarrange:
plotsleft = list(ppplots[[1]],ppplots[[2]],ppplots[[3]],
                 ppplots[[4]],ppplots[[5]],
                 ppplots[[8]],ppplots[[9]],ppplots[[10]],
                 ppplots[[11]],ppplots[[12]],
                 ppplots[[7]],ppplots[[13]],ppplots[[14]],
                 ppplots[[15]],ppplots[[16]])
plotsright=list(ppplots[[6]], NULL,legend,NULL)
ggarrange(
  ggarrange(plotlist=plotsleft, nrow=3, ncol=5), 
  ggarrange(plotlist=plotsright, nrow=4, ncol=1, heights=c(3,1,4,1)),
  nrow=1, ncol=2, widths=c(5,1))



