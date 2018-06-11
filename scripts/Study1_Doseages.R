# Microdosing Experiences
# 11 June 18
# Vince Polito
# vince.polito@mq.edu.au
# 


doseage<-read_csv("cleandata/dosesrecoded.csv")

# Generate a table of doses for substances reported in study1
# Exclude high doses
doseage<-filter(doseage, (!ResponseID %in% read_csv("cleandata/hidose.csv")$ResponseID)) %>%
  group_by(Reporting_Category) %>%
  summarise(count=n(), 
            MeanDose=mean(Recoded_Dose,na.rm=TRUE),
            SD=sd(Recoded_Dose,na.rm=TRUE),
            Min=min(Recoded_Dose,na.rm=TRUE),
            Max=max(Recoded_Dose,na.rm=TRUE)) %>%
  arrange(-count)
doseage
