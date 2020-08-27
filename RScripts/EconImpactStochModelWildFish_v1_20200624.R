##########
#
# EPIC4 Economic impact stochastic model - for wild fisheries
#
# by Travis Tai, Annie Mejaes
# 
# Date: 2020/06/24
#
##########

rm(list=ls())
setwd("/Users/travistai/Documents/GitHub/Epic4/")  ## set working directory to location of files

## read in functions and libraries
source("./RScripts/Functions/ipak_func.R")  
source("./RScripts/StockRecrFunc.R")
pckgs<-c("tidyverse","ggplot2")
ipak(pckgs)

## read in functions

##### READ DATA ##### 

## fisheries net profit data  
fishprof.dat<-read.csv("./Data/Scen6NetProfitRicker.csv",header=T,strip.white=T,stringsAsFactors=F)
head(fishprof.dat)

## multipliers data
mult.dat<-read.csv("./Data/Multipliers_20200729.csv",header=T,strip.white=T,stringsAsFactors=F) 
head(mult.dat)
tempmult.dat<-mult.dat
mult.dat<-tempmult.dat %>% gather("Year","Multiplier",4:10) %>%
  group_by(Economic.Indicators,Path,Sector) %>%
  summarize(Mult_Mean = mean(Multiplier,na.rm=T),
            Mult_SD = sd(Multiplier,na.rm=T))
write.csv(mult.dat,"./Tables/MultiplerTableMeanSD.csv",row.names = F)
  
## hatchery release data
model.dat<-read.csv("./Data/SimModelValues.csv",header=T,strip.white=T,stringsAsFactors=F)

## stock recruit parameter data 
storec.dat<-read.csv("./Data/StockRecruitValues.csv",header=T,strip.white=T,stringsAsFactors=F)



##### END READ DATA ##### 





##### PARAMETERS #####

## simulation parameters
set.seed(1)  ## set random seed to start at the same point (for reproducability of stochastic model)
iter<-10000  ## number of iterations - do 10000 then sample from 10000

n_years<-2014:2050


## model value parameters
d_rate<-0.08  ## discount rate: Canada = 8%
d_year<-2020  ## discount year start; Bendriem start = 2018; we start at 2020

rel_Year<-2020  ## relative year to start harvesting when survival increases
mult.cate<-unique(mult.dat$Economic.Indicators)  ## multiplier categories
mean_bodysize<-3.46  ## mean body size of coho kg
exv_price<-3.02  ## ex vessel price $/kg
costfishing_weekly<-3391.2  ## fishing cost $/week
Spawn2011to13<-c(25487,54832,58641)  ## historical spawners data: 2011-2013
tar_esc<-40000  ## target escapement

## stock recruit parameters  CHECK TABLE storec.dat TO ENSURE CORRECT VALUES
alphalo_mean<-storec.dat$Value[1]
alphalo_SE<-storec.dat$StandardError[1]
xvar1lo_mean<-storec.dat$Value[2]
xvar1lo_SE<-storec.dat$StandardError[2]
alphahi_mean<-storec.dat$Value[3]
alphahi_SE<-storec.dat$StandardError[3]
xvar1hi_mean<-storec.dat$Value[4]
xvar1hi_SE<-storec.dat$StandardError[4]

##### END PARAMETERS #####





##### SIMULATION START #####



stockrec.out<-list() ## empty matrix for stock recruit data (row = years; column = variables; z = iteration)

econ.out<-list() ## create empty list (rows = years #; column = econ.indicator; z = iterations)

iter.ind<-as.data.frame(matrix(nrow=iter,ncol=12)) ## data frame for iteration indicator analysis
names(iter.ind)<-c("iter",
                   "harvest_areaG","total_escape","total_return","wild_return",
                   "harvest_annual",
                   mult.cate,
                   "netpresentvalue","netprofit_annual")

for(i in 1:iter){  ## stochastic stock recruit component
  
  ## set stochastic stock recruit parameter
  ## run stock recruit simulation
  tout.dat<-stockRecr(init.spawn = Spawn2011to13,years = n_years,
                      sim.dat = model.dat,
                      model="Ricker",
                      relYr = rel_Year) %>% 
    mutate(total_return = age3_ret + hatch_return,
           harvest = ifelse(Year>=rel_Year+2,exp_rate*total_return,0),
           harvest_areaG = harvest*0.9) %>%
    left_join((model.dat %>% select(Year,Catchability)),by = "Year") %>%
    mutate(effort = harvest_areaG/(Catchability*age3_ret)) %>%
    replace_na(list(Catchability=0,effort=0)) %>%
    mutate(LV = harvest_areaG * exv_price * mean_bodysize,
           fishingcost_season = ifelse(Year>=rel_Year+2,effort*costfishing_weekly+120000,0),
           net_revenue = LV - fishingcost_season,
           implement_cost = ifelse(Year>=rel_Year,9200,0),
           net_profit = net_revenue-implement_cost) 
  
  ## store stock recruit data
  stockrec.out[[i]]<-tout.dat 

}

for(i in 1:iter){  ## stochastic economic multipliers component
  
  ## subset net profit data, and setup discount year start = 2020
  #tfishprof.dat<-tout.dat %>% select(Year,net_profit) %>% 
  #  filter(Year>=n_years[1]) %>% 
  #  mutate(t_init = ifelse(Year>=d_year,Year-d_year,0))
  
  tfishprof.dat<-stockrec.out[[i]] %>% select(Year,net_profit) %>% 
    filter(Year>=n_years[1]) %>% 
    mutate(t_init = ifelse(Year>=d_year,Year-d_year,0))
  
  tyears<-tfishprof.dat$Year
  tout.dat2<-data.frame()
  for(j in 1:length(tyears)){  ## generate random multipler from mean and SD
    tmult.dat<-data.frame(t.Mult=-1)
    while(any(tmult.dat$t.Mult<0)){
    tmult.dat<-as.data.frame(mult.dat) %>% mutate(t.Mult = apply(mult.dat,1,FUN = function(x){
      rnorm(1, mean = as.numeric(x[4]), sd = as.numeric(x[5]))})) 
    }
    tmult.dat<-tmult.dat %>%
      filter(Sector %in% c("Fisheries","Processing")) %>%
      select(-Mult_Mean,-Mult_SD) %>%
      mutate(Year = tyears[j],
             net_profit = tfishprof.dat$net_profit[j],
             t_init = tfishprof.dat$t_init[j]) 
    tout.dat2<-tout.dat2 %>% bind_rows(tmult.dat)
  }
  
  ## estimate NPV with random generated multipler
  tout.dat2<-tout.dat2 %>% 
    mutate(NPV = net_profit*t.Mult/(1+d_rate)^t_init) %>%
    filter(Year >= rel_Year) %>%
    select(-t.Mult,-t_init,-net_profit)
  econ.out[[i]]<-tout.dat2
  
  rm(tout.dat2,tmult.dat)
}

for(i in 1:iter){  ## output of indicators over entire time series
  
  ## subset stock recruit data
  tdat<-stockrec.out[[i]] %>% 
    mutate(t_init = ifelse(Year>=d_year,Year-d_year,0))
    
  ## store summed time series iteration data
  temp.iter.dat<-tdat %>% select(harvest_areaG,total_escape,total_return,age3_ret) %>%
    summarize(harvest_areaG = sum(harvest_areaG,na.rm=T),
              total_escape = sum(total_escape,na.rm=T),
              total_return = sum(total_return,na.rm=T),
              wild_return = sum(age3_ret,na.rm=T)) %>%
    bind_cols(tdat %>% filter(Year>=2020) %>% 
                summarize(harvest_annual = mean(harvest_areaG,na.rm=T)))
  
  ## econ data subset
  tecon<-econ.out[[i]]  %>% 
    group_by(Economic.Indicators,Year) %>%
    summarize(NPV = sum(NPV))
  
  iter.ind[i,1]<-i
  iter.ind[i,2:6]<-temp.iter.dat
  iter.ind[i,7]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Employment")])  
  iter.ind[i,8]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="GDP")])
  iter.ind[i,9]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="LabourIncome")])
  iter.ind[i,10]<-sum(tecon$NPV[which(tecon$Economic.Indicators=="Output")])
  iter.ind[i,11]<-sum((tdat$net_profit/(1+d_rate)^tdat$t_init),na.rm=T)
  iter.ind[i,12]<-mean(tdat$net_profit,na.rm=T)
  
  rm(tdat,temp.iter.dat,tecon)
}


##### END SIMULATION #####



##### TESTING FOR OPTIMAL NUMBER OF ITERATIONS #####
set.seed(100)
c.iter<-rep(seq(100,10000,100),each=1)

conv.dat<-data.frame()

for(i in 1:length(c.iter)){
  tsample.dat<-iter.ind %>% sample_n(c.iter[i],replace=T) %>%
    summarize(iterations = c.iter[i],
              netprof_mean = mean(netpresentvalue,na.rm=T),
              netprof_SE = sd(netpresentvalue,na.rm=T)/sqrt(c.iter[i]),
              netprof_50 = quantile(netpresentvalue,0.5,na.rm=T),
              netprof_70 = quantile(netpresentvalue,0.7,na.rm=T),
              netprof_90 = quantile(netpresentvalue,0.9,na.rm=T),
              netprof_95 = quantile(netpresentvalue,0.95,na.rm=T),
              annnetprof_mean = mean(netprofit_annual,na.rm=T),
              annnetprof_SE = sd(netprofit_annual,na.rm=T)/sqrt(c.iter[i]))
  conv.dat<-conv.dat %>% bind_rows(tsample.dat)
  rm(tsample.dat)
}


plot.dat<-conv.dat %>%
  mutate(permean = abs(100*(netprof_mean-netprof_mean[length(c.iter)])/
                         netprof_mean[length(c.iter)]),
         per_ann = abs(100*(annnetprof_mean-annnetprof_mean[length(c.iter)])/
                         annnetprof_mean[length(c.iter)]),
         per50 = abs(100*(netprof_50-netprof_50[length(c.iter)])/
                       netprof_50[length(c.iter)]),
         per70 = abs(100*(netprof_70-netprof_70[length(c.iter)])/
                       netprof_70[length(c.iter)]),
         per90 = abs(100*(netprof_90-netprof_90[length(c.iter)])/
                       netprof_90[length(c.iter)]),
         per95 = abs(100*(netprof_95-netprof_95[length(c.iter)])/
                       netprof_95[length(c.iter)])) %>%
  gather("percentile", "value",12:15)
         



p.iter<-ggplot(plot.dat)
p.iter + geom_line(aes(x = iterations, y=netprof_mean))
p.iter + geom_line(aes(x = iterations, y=netprof_SE))
p.iter + geom_line(aes(x = iterations, y=annnetprof_mean))
p.iter + geom_line(aes(x = iterations, y=annnetprof_SE))

p1f<-p.iter + theme(axis.title = element_text(size = 8),
                    axis.text = element_text(size = 7),
                    legend.text = element_text(size = 7),
                    legend.title = element_text(size=7)) + 
  geom_line(aes(x = iterations, y = value, col = percentile),lwd=0.25) + 
  geom_smooth(aes(x = iterations, y = value, col = percentile),method = "loess",lwd=0.6) +
  geom_vline(xintercept = 3000, lty = 2, lwd= 0.2) +
  labs(x = "Iterations",
       y = "Absolute % difference from 10k\niteration results") +
  scale_colour_discrete(name = "Percentile", labels = c("50th","70th","90th","95th"))

pdf("./Plots/IterConvAnalysis.pdf",width = 3.5, height = 2)
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
opt.iter<-3000

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

write.csv(sum.dat,"./Tables/CaptureFisheries_ModSimOutput.csv",row.names=F)




##### PLOT OUTPUTS #####

## stock-rec plots ##
stockMean.dat<-as.data.frame(matrix(NA,nrow=0,ncol=4))
names(stockMean.dat)<-c("Year","Ind","y_FishMean","y_FishSE")

tvar.dat<-array(NA,dim=c(length(n_years)+3,4,length(n_samp)))
for(i in 1:length(n_samp)){
  tvar.dat[,,i]<-as.matrix(stockrec.out[[n_samp[i]]] %>% #filter(Year>2013) %>%
                             select(age3_ret,total_return,total_escape,harvest_areaG))
}

storec.categ<-c("Wild return","Total return","Total escapement","Harvest - area G")
for(i in 1:4){
  tdat<-data.frame(Year = c(2011:2013,n_years), 
                   Ind = storec.categ[i], 
                   y_FishMean = apply(tvar.dat[,i,],1,mean),
                   y_FishSE = (apply(tvar.dat[,i,],1,sd)/sqrt(length(n_samp))))
  stockMean.dat<-stockMean.dat %>% bind_rows(tdat)
  rm(tdat)
}
rm(tvar.dat)
stockMean.dat$Ind<-factor(stockMean.dat$Ind,
                          levels = c("Wild return","Total return",
                                     "Total escapement","Harvest - area G"))
stockMean.dat<-stockMean.dat %>% filter(Year >= rel_Year)

## plot stock recruit data
p2.1<-ggplot(data=stockMean.dat)
p2.1f<-p2.1 + theme(axis.title = element_text(size = 8),
                    axis.text = element_text(size = 7),
                    legend.text = element_text(size = 7),
                    legend.title = element_text(size=7),
                    strip.text = element_text(size=6)) + 
  geom_line(aes(x=Year, y=y_FishMean/1000, colour=Ind)) +
  geom_ribbon(aes(x=Year, ymin=(y_FishMean-(1.96*y_FishSE))/1000, 
                  ymax=(y_FishMean+(1.96*y_FishSE))/1000),
              fill="grey50",alpha=0.5) +
  facet_wrap(stockMean.dat$Ind,nrow=2,scales="free") +
  labs(x="Year",y="# fish (thousand)") + 
  scale_colour_discrete(guide=F)

pdf("./Plots/StockRec_ModSimOutput.pdf",width=3.5,height=3)
p2.1f
dev.off()

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




pdf("./Plots/EconIndicator_ModSimOutput.pdf",width=3.5,height=3)
p3.1f
dev.off()







