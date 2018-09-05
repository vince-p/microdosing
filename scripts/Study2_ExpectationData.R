# Microdosing Experiences
# Vince Polito
# vince.polito@mq.edu.au
# 
# This script relies on csv files in the cleandata subfolder
# The final part will only work if run after LongtemLME script

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, psych, weights, scales)
p_load_gh("vince-p/vtools")

expect.totals<-read_csv("cleandata/expect_totals.csv")

psych::describe(expect.totals)

direction<-expect.totals[ , c(-1,-grep("_w",colnames(expect.totals)))] # This is a dataframe of all the direction variables (ie -1, 0 or 1)
weights<-dplyr::select(expect.totals,experience,ends_with("_w"))

# this uses mapply to apply the function using two changing variables. 
# Code from: https://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
wt<-function(var1,var2) # Temporary function to take two variables (directions and weights) and calculated weighted mean and t score
{
  x<-wtd.mean(x=var1,weight=var2) #calculate the weighted mean
  z<-wtd.t.test(x=var1,weight=var2) #calculate the weighted t-test
  d<-ES.t.one(m=z$additional[2],se=z$additional[4],n=z$coefficients[2]+1,mu=0)$d
  c(x,z$coefficients,d) #output the weighted mean, the weighted t-test results, the d score for each test
}
#wt(expect.totals$DASS_Anxiety,expect.totals$DASS_Anxiety_w) #This line is to test the function

naive<-t(mapply(wt,direction[direction$experience==2,-1],weights[weights$experience==2,-1])) #generate weights for naive only
naive<-tibble(variable=rownames(naive),wtdmean=naive[,1],rank=rank(-(as.data.frame(naive[,1])))) #make tibble with ranks

exp<-t(mapply(wt,direction[direction$experience==1,-1],weights[weights$experience==1,-1])) #generate weights for exp only
exp<-tibble(variable=rownames(exp),wtdmean=exp[,1],rank=rank(-(as.data.frame(exp[,1])))) #make tibble with ranks

#Single table with naive and experienced weightings, for table S3
round_df(cbind(naive,exp[2:3]))


#Test association between naive and experienced particpants
cor.test(x=exp$rank, y=naive$rank, method = 'kendall')
# Output: tau=.934, p <.001

expect.results<-t(mapply(wt,direction[,-1],weights[,-1])) # This outputs a matrix so col labels are weird. Not sure why this must be done this way.
expect.results<-tibble(variable=rownames(expect.results),
                       wtdmean=expect.results[,1],
                       t=expect.results[,2],
                       df=expect.results[,3],
                       p=pv(expect.results[,4]),
                       d=expect.results[,5],
                       rank=rank(-(as.data.frame(expect.results[,1]))),
                       checkvar=pp.uncorrected$k,
                       checkT=pp.uncorrected$T,
                       rankStudy1=rank((pp.uncorrected$T))) #make tibble with ranks
expect.neat<-round_df(expect.results)

cor.test(x=expect.results$rank, y=expect.results$rankStudy1, method = 'kendall') 
