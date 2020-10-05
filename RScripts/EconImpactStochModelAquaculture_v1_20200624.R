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
write.csv(mult.dat,"./Tables/MultiplerTableAquacultureMeanSD.csv",row.names = F)

##### END READ DATA ##### 





##### PARAMETERS #####

## simulation parameters
set.seed(1)  ## set random seed to start at the same point (for reproducability of stochastic model)
iter<-10000  ## number of iterations - do 10000 then sample from 10000

n_years<-2020:2050


## model value parameters
d_rate<-0.058  ## discount rate: Canada = 8%, less 2.2% inflation
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
    mutate(NPV = net_profit*t.Mult/(1+d_rate)^t_init) %>%
    filter(Year >= rel_Year) %>%
    select(-t.Mult,-t_init,-net_profit)
  econ.out[[i]]<-tout.dat2
  
  rm(taquaprof.dat,tout.dat2,tmult.dat)
}

for(i in 1:iter){  ## output of indicators over entire time series
  
  taquaprof.dat<-aquaprof.dat %>% select(Year,t_init,net_profit) 
  
  ## econ data subset
  tecon<-econ.out[[i]]  %>% 
    group_by(Economic.Indicators,Year) %>%
    summarize(NPV = sum(NPV))
  
  iter.ind[i,1]<-i
  iter.ind[i,2]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Employment")])  
  iter.ind[i,3]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="GDP")])
  iter.ind[i,4]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="LabourIncome")])
  iter.ind[i,5]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Output")])
  iter.ind[i,6]<-sum((taquaprof.dat$net_profit/(1+d_rate)^taquaprof.dat$t_init),na.rm=T)
  iter.ind[i,7]<-mean(aquaprof.dat$net_profit,na.rm=T)
  
  rm(taquaprof.dat,tecon)
}


##### END SIMULATION #####



##### TESTING FOR OPTIMAL NUMBER OF ITERATIONS #####
set.seed(100)
c.iter<-rep(seq(100,10000,100),each=1)
#c.iter<-rep(seq(100,1000,100),each=1)
conv.dat<-data.frame()

for(i in 1:length(c.iter)){
  tsample.dat<-iter.ind %>% sample_n(c.iter[i],replace=T) %>%
    mutate(totalSum = Employment+GDP+LabourIncome+Output) %>%
    summarize(iterations = c.iter[i],
              totalSum_mean = mean(totalSum,na.rm=T),
              totalSum_SE = sd(totalSum,na.rm=T)/sqrt(c.iter[i]),
              totalSum_50 = quantile(totalSum,0.5,na.rm=T),
              totalSum_70 = quantile(totalSum,0.7,na.rm=T),
              totalSum_90 = quantile(totalSum,0.9,na.rm=T),
              totalSum_95 = quantile(totalSum,0.95,na.rm=T),
              annnetprof_mean = mean(netprofit_annual,na.rm=T),
              annnetprof_SE = sd(netprofit_annual,na.rm=T)/sqrt(c.iter[i]))
  conv.dat<-conv.dat %>% bind_rows(tsample.dat)
  rm(tsample.dat)
}

plot.dat<-conv.dat %>%
  mutate(permean = abs(100*(totalSum_mean-totalSum_mean[length(c.iter)])/
                         totalSum_mean[length(c.iter)]),
         per_ann = abs(100*(annnetprof_mean-annnetprof_mean[length(c.iter)])/
                         annnetprof_mean[length(c.iter)]),
         per50 = abs(100*(totalSum_50-totalSum_50[length(c.iter)])/
                       totalSum_50[length(c.iter)]),
         per70 = abs(100*(totalSum_70-totalSum_70[length(c.iter)])/
                       totalSum_70[length(c.iter)]),
         per90 = abs(100*(totalSum_90-totalSum_90[length(c.iter)])/
                       totalSum_90[length(c.iter)]),
         per95 = abs(100*(totalSum_95-totalSum_95[length(c.iter)])/
                       totalSum_95[length(c.iter)])) %>%
  gather("percentile", "value",12:15)

p.iter<-ggplot(plot.dat)
p.iter + geom_line(aes(x = iterations, y=totalSum_mean))
p.iter + geom_line(aes(x = iterations, y=totalSum_SE))
p.iter + geom_line(aes(x = iterations, y=annnetprof_mean))
p.iter + geom_line(aes(x = iterations, y=annnetprof_SE))

p1f<-p.iter + theme(axis.title = element_text(size = 8),
                    axis.text = element_text(size = 7),
                    legend.text = element_text(size = 7),
                    legend.title = element_text(size=7)) + 
  geom_line(aes(x = iterations, y = value, col = percentile),lwd=0.25) + 
  geom_smooth(aes(x = iterations, y = value, col = percentile),method = "loess",lwd=0.6) +
  geom_vline(xintercept = 2000, lty = 2, lwd= 0.2) +
  labs(x = "Iterations",
       y = "Absolute % difference from 10k\niteration results") +
  scale_colour_discrete(name = "Percentile", labels = c("50th","70th","90th","95th"))

pdf("./Plots/AquaIterConvAnalysis.pdf",width = 3.5, height = 2)
p1f
dev.off()

rm(conv.dat,plot.dat,p.iter,p1f)



##
##
##
##
##### Secondary sampling for optimal iteration data ##### 
##
##
##
##

set.seed(100)
opt.iter<-2000

n_samp<-sample(c(1:iter),opt.iter,replace = F)
sum.dat<-data.frame()

for(i in n_samp){
  tdat<-econ.out[[i]] %>% 
    group_by(Economic.Indicators,Path,Sector) %>%
    summarize(NPV = sum(NPV, na.rm=T))
  tdat2<-tdat %>% group_by(Economic.Indicators) %>%
    summarize(NPV = sum(NPV, na.rm=T)) %>%
    mutate(Path = "Total")
  tdat<-tdat %>% bind_rows(tdat2)
  if(i==n_samp[1]){
    sum.dat<-tdat
  } else {
    sum.dat<-sum.dat %>% 
      left_join(tdat,by = c("Economic.Indicators","Path","Sector")) 
  }
  rm(tdat,tdat2)
}

sumoutname.dat<-sum.dat[,c(1,2,3)]
sumout.dat<-sum.dat[,c(-1,-2,-3)]

sum.dat$NPV_mean<-apply(sumout.dat,1,mean)
sum.dat$NPV_SE<-apply(sumout.dat,1, function(x){sd(x)/sqrt(opt.iter)})
sum.dat<-sum.dat %>% select(Economic.Indicators,Path,Sector,NPV_mean,NPV_SE) %>%
  mutate(NPV_CI95 = NPV_SE*1.96)

write.csv(sum.dat,"./Tables/Aquaculture_ModSimOutput.csv",row.names=F)


##### PLOT OUTPUTS #####

## econ plots ##

## generate mean and standard error for simulation to plot 
econMean.dat<-data.frame()
for(i in n_samp){
  tdat<-econ.out[[i]] %>%
    group_by(Economic.Indicators,Year) %>%
    summarize(NPV = sum(NPV,na.rm=T))
  if(i==n_samp[[1]]){
    econMean.dat<-tdat
  } else {
    econMean.dat<-econMean.dat %>%
      left_join(tdat, by = c("Economic.Indicators","Year"))
  }
  rm(tdat)
}

teconMean<-econMean.dat[,c(-1,-2)]
econMean.dat$NPV_mean<-apply(teconMean,1,mean,na.rm=T)
econMean.dat$NPV_SE<-apply(teconMean,1,function(x){sd(x)/sqrt(opt.iter)})
econMean.dat<-econMean.dat %>% select(Economic.Indicators,Year,NPV_mean,NPV_SE) %>%
  mutate(NPV_CI95 = NPV_SE*1.96)
rm(teconMean)

econMean.dat$Economic.Indicators<-factor(econMean.dat$Economic.Indicators,levels=mult.cate)


## plot economic data
p3.1<-ggplot(data=econMean.dat)
p3.1f<-p3.1 + theme(axis.title = element_text(size = 8),
                    axis.text = element_text(size = 7),
                    legend.text = element_text(size = 7),
                    legend.title = element_text(size=7),
                    strip.text = element_text(size=6)) + 
  geom_line(aes(x=Year, y=NPV_mean/1000, colour=Economic.Indicators)) +
  geom_ribbon(aes(x=Year, ymin=(NPV_mean-(1.96*NPV_SE))/1000, 
                  ymax=(NPV_mean+(1.96*NPV_SE))/1000),
              fill="grey50",alpha=0.5) +
  labs(x="Year",y="Net present value (1000 USD)") +
  facet_wrap(econMean.dat$Economic.Indicators,nrow=2,scales="free") +
  scale_colour_discrete(guide=F)




pdf("./Plots/AquaEconIndicator_ModSimOutput.pdf",width=3.5,height=3)
p3.1f
dev.off()





