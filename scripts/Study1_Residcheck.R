# Microdosing Experiences
# Vince Polito
# vince.polito@mq.edu.au
# 
# This script relies on csv files in the cleandata subfolder
# This script checks residuals for the both sets of linear models (ie for daily and long term analyses) in Study1

#vc function performs a check of residuals in a  linear model
vc<-function(lmemodel,ind){
  plot(ind, resid(lmemodel)) #first plot residuals by type
  
  # More analytically:
  print(by(resid(lmemodel), INDICES = ind, FUN=sd)) #show data in text format
  # Look for no more than 2x difference between largest and smallest sd.
  
  ##### Checking for normality
  qqnorm(resid(lmemodel)) 
  }

dailydata<-read.csv("cleandata/dailydata.csv") #Load daily summary data
#dailydata<-dailydata[!dailydata$id %in% read_csv("cleandata/hidose.csv")$id,] ## RUN THIS LINE TO EXCLUDE HIGH DOSES (SEE PAGE 17)
# This section calls the function for each daily rating
vc(lme(d.connected~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.contemplat~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.creative~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.focus~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.happy~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.productive~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)
vc(lme(d.well~type,random=~1|id,data=dailydata,method="ML",na.action = na.omit),ind=dailydata$type)

longtermdata<-read.csv("cleandata/longterm.csv") #Load longterm data
# This section calls the function for each longterm variable
vc(lme(DASS_Depression~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(DASS_Anxiety~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(DASS_Stress~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(MWQ_Mean~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(QOLI_Raw~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata, na.action = na.exclude),ind=longtermdata$Time)
vc(lme(MAAS_Mean~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(HMS_Total~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(M5_Extraversion~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(M5_Agreeableness~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(M5_Conscientiousness~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(M5_Neuroticism~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(M5_Openess~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(TAS_Total~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(CPS_Total~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(SOARS_Involuntariness~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
vc(lme(SOARS_Effortlessness~Time*experience+Time*maxdoses.c, random=~1|id, ,method="ML",data=longtermdata),ind=longtermdata$Time)
