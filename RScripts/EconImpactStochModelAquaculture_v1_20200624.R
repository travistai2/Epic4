##########
#
# EPIC4 Economic impact stochastic model - for aquaculture production
#
# by Travis Tai, Annie Mejaes
# 
# Date: 2020/08/26
#
##########

rm(list=ls())
setwd("/Users/travistai/Documents/GitHub/Epic4/")  ## set working directory to location of files

## read in functions and libraries
source("./RScripts/Functions/ipak_func.R")  
pckgs<-c("tidyverse","ggplot2")
ipak(pckgs)

## read in functions

##### READ DATA ##### 

## aquaculture net profit data  
aquaprof.dat<-read.csv("./Data/NetProfitAquaculture20200826.csv",header=T,strip.white=T,stringsAsFactors=F) %>%
  filter(Scenario=="AllTraits") %>%
  rename(net_profit = PV)
head(aquaprof.dat)

## multipliers data
mult.dat<-read.csv("./Data/Multipliers_20200729.csv",header=T,strip.white=T,stringsAsFactors=F) 
head(mult.dat)
tempmult.dat<-mult.dat
mult.dat<-tempmult.dat %>% gather("Year","Multiplier",4:10) %>%
  group_by(Economic.Indicators,Path,Sector) %>%
  summarize(Mult_Mean = mean(Multiplier,na.rm=T),
            Mult_SD = sd(Multiplier,na.rm=T))

##### END READ DATA ##### 





##### PARAMETERS #####

## simulation parameters
set.seed(1)  ## set random seed to start at the same point (for reproducability of stochastic model)
iter<-10000  ## number of iterations - do 10000 then sample from 10000

n_years<-2020:2050


## model value parameters
d_rate<-0.08  ## discount rate: Canada = 8%
d_year<-2020  ## discount year start; Bendriem start = 2018; we start at 2020

rel_Year<-2020  ## relative year to start harvesting when survival increases
mult.cate<-unique(mult.dat$Economic.Indicators)  ## multiplier categories

##### END PARAMETERS #####




##### SIMULATION START #####

econ.out<-list() ## create empty list (rows = years #; column = econ.indicator; z = iterations)

iter.ind<-as.data.frame(matrix(nrow=iter,ncol=7)) ## data frame for iteration indicator analysis
names(iter.ind)<-c("iter",
                   mult.cate,
                   "netpresentvalue","netprofit_annual")

p1<-ggplot()  ## set up ggplot to plot economic results



for(i in 1:iter){  ## stochastic economic multipliers component
  
  ## subset net profit data, and setup discount year start = 2020
  
  taquaprof.dat<-aquaprof.dat %>% select(Year,t_init,net_profit) 
  
  tyears<-taquaprof.dat$Year
  tout.dat2<-data.frame()
  for(j in 1:length(tyears)){  ## generate random multipler from mean and SD
    tmult.dat<-data.frame(t.Mult=-1)
    while(any(tmult.dat$t.Mult<0)){
      tmult.dat<-as.data.frame(mult.dat) %>% mutate(t.Mult = apply(mult.dat,1,FUN = function(x){
        rnorm(1, mean = as.numeric(x[4]), sd = as.numeric(x[5]))})) 
    }
    tmult.dat<-tmult.dat %>%
      filter(Sector %in% c("Aquaculture","Processing")) %>%
      select(-Mult_Mean,-Mult_SD) %>%
      mutate(Year = tyears[j],
             net_profit = taquaprof.dat$net_profit[j],
             t_init = taquaprof.dat$t_init[j]) 
    tout.dat2<-tout.dat2 %>% bind_rows(tmult.dat)
  }
  
  ## estimate NPV with random generated multipler
  tout.dat2<-tout.dat2 %>% 
    mutate(NPV = net_profit*t.Mult/(1+d_rate)^t_init)
  econ.out[[i]]<-tout.dat2
  
  rm(tout.dat2,tmult.dat)
}

for(i in 1:iter){  ## output of indicators over entire time series
  
  ## econ data subset
  tecon<-econ.out[[i]]  %>% 
    group_by(Economic.Indicators,Year) %>%
    summarize(NPV = sum(NPV))
  
  iter.ind[i,1]<-i
  iter.ind[i,2]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Employment")])  
  iter.ind[i,3]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="GDP")])
  iter.ind[i,4]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="LabourIncome")])
  iter.ind[i,5]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Output")])
  iter.ind[i,6]<-sum((tdat$net_profit/(1+d_rate)^tdat$t_init),na.rm=T)
  iter.ind[i,7]<-mean(tdat$net_profit,na.rm=T)
  
  rm(tdat,temp.iter.dat,tecon)
}




