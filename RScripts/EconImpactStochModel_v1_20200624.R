##########
#
# EPIC4 Economic impact stochastic model
#
# by Travis Tai, Annie Mejaes
# 
# Date: 2020/06/24
#
##########

rm(list=ls())
setwd("/Users/travistai/Documents/GitHub/Epic4/")  ## set working directory to location of files

## read in libraries
source("/Users/travistai/Documents/GitHub/Epic4/RScripts/Functions/ipak_func.R")  
pckgs<-c("tidyverse")
ipak(pckgs)

##### READ DATA ##### 

## fisheries net profit data  
fishprof.dat<-read.csv("./Data/Scen6NetProfitRicker.csv",header=T,strip.white=T,stringsAsFactors=F)
head(fishprof.dat)

## aquaculture net profit data  


## multipliers data
mult.dat<-read.csv("./Data/Multipliers_20200624.csv",header=T,strip.white=T,stringsAsFactors=F) %>% 
  mutate(Mult_SD = Multiplier*0.2)
head(mult.dat)

##### END READ DATA ##### 





##### PARAMETERS #####

## simulation parameters
set.seed(100)  ## set random seed to start at the same point (for reproducability of stochastic model)
iter<-100  ## number of iterations

## model value parameters
d_rate<-0.08  ## discount rate
mult.cate<-unique(mult.dat$Economic.Indicators)  ## multiplier categories

##### END PARAMETERS #####




##### SIMULATION START #####

output.dat<-array(NA,dim=c(nrow(fishprof.dat),iter,length(mult.cate))) ## create empty output matrix (rows = years #; column = iterations; z = econ.indiator)
p1<-ggplot()  ## set up ggplot to plot results

for(i in 1:iter){
  tfishprof.dat<-fishprof.dat
  
  years<-tfishprof.dat$Year
  tout.dat<-data.frame()
  for(j in 1:length(years)){  ## generate random multipler from mean and SD
    tmult.dat<-mult.dat %>% mutate(t.Mult = apply(mult.dat,1,FUN = function(x){
      rnorm(1, mean = as.numeric(x[4]), sd = as.numeric(x[5]))})) %>%
      group_by(Economic.Indicators,Sector) %>%
      summarize(t.MultTotal = sum(t.Mult)-1) %>% 
      filter(Sector=="Fisheries") %>%
      select(-Sector) %>%
      mutate(Year = years[j]) %>%
      spread(Economic.Indicators,t.MultTotal,)
    tout.dat<-tout.dat %>% bind_rows(tmult.dat)
  }
  
  output.dat[,i,1]<-tout.dat$Output * tfishprof.dat$Total.Net.Profit*(1+d_rate)^tfishprof.dat$t_init
  output.dat[,i,2]<-tout.dat$GDP * tfishprof.dat$Total.Net.Profit*(1+d_rate)^tfishprof.dat$t_init
  output.dat[,i,3]<-tout.dat$LabourIncome * tfishprof.dat$Total.Net.Profit*(1+d_rate)^tfishprof.dat$t_init
  output.dat[,i,4]<-tout.dat$Employment * tfishprof.dat$Total.Net.Profit*(1+d_rate)^tfishprof.dat$t_init
  
  tplot.dat<-data.frame(Year = rep(years,4),
                        Ind = rep(mult.cate,each=length(years)),
                        NPV = c(output.dat[,i,1],output.dat[,i,2],output.dat[,i,3],output.dat[,i,4]))
  
  p1 <- p1 + geom_line(data=tplot.dat, aes(x=Year, y=NPV, col=Ind),lwd=0.05) + 
    facet_wrap(tplot.dat$Ind,nrow=2,scales="free")
  rm(tout.dat,tmult.dat)
}

outputMean.dat<-as.data.frame(matrix(NA,nrow=0,ncol=3))
names(outputMean.dat)<-c("Year","Ind","NPV")
for(i in 1:4){
  tdat<-data.frame(Year = years, Ind = mult.cate[i], NPV = apply(output.dat[,,i],1,mean),
                   NPV_SE = (apply(output.dat[,,i],1,sd)/sqrt(iter)))
  outputMean.dat<-outputMean.dat %>% bind_rows(tdat)
  rm(tdat)
}
outputMean.dat$Ind<-as.factor(outputMean.dat$Ind)


p1 + geom_line(data=outputMean.dat, aes(x=Year, y=NPV, colour=Ind)) +
  geom_ribbon(data=outputMean.dat, 
              aes(x=Year, ymin=NPV-(1.96*NPV_SE), ymax=NPV+(1.96*NPV_SE)),
              fill="grey50",alpha=0.5) +
  facet_wrap(outputMean.dat$Ind,nrow=2,scales="free") 





##### END SIMULATION #####