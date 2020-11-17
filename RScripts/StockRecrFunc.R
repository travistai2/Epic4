
#
# EPIC4 Stock recruit function
#
# by Travis Tai, Annie Mejaes
# 
# Date: 2020/08/04
#

###### STOCK RECRUIT MODEL FUNCTION #####
stockRecr<-function(init.spawn,init.totescape,
                    years,
                    sim.dat = model.dat,
                    model=c("Ricker","BevHolt"),
                    relYr = 2020){
  if(length(init.spawn)!=3){
    stop("require length init.spawn = 3")
  }
  init.years<-c((min(years)-3):(min(years)-1))
  tout.dat<-data.frame(Year = 2015:2017, alpha=0, beta=0, 
                       age3_ret = 0, exp_rate = 0, wild_escape = 0,
                       hatchSmoltRel = 0, hatch_surv = 0, hatch_return = 0, hatch_escape = 0, 
                       total_escape = init.totescape)
  
  for(i in 1:length(years)){
    
    ## generate stochastic stock recruit parameters 
    alpha_lo<-rnorm(1,alphalo_mean,alphalo_SE)
    xvar1_lo<-rnorm(1,xvar1lo_mean,xvar1lo_SE)
    #while(xvar1_lo>=0){
    #  xvar1_lo<-rnorm(1,xvar1lo_mean,xvar1lo_SE)
    #}
    beta_lo<--alpha_lo/xvar1_lo
    alpha_hi<-rnorm(1,alphahi_mean,alphahi_SE)
    xvar1_hi<-rnorm(1,xvar1hi_mean,xvar1hi_SE)
    #while(xvar1_hi>=0){
    #  xvar1_hi<-rnorm(1,xvar1hi_mean,xvar1hi_SE)
    #}
    beta_hi<--alpha_hi/xvar1_hi
    
    tescape<-tout.dat$total_escape[i]  ## total escapement
    if(years[i]>=relYr){
      talpha<-alpha_hi
      tbeta<-beta_hi
    } else {
      talpha<-alpha_lo
      tbeta<-beta_lo
    }
    if(model=="Ricker"){
      tdat<-data.frame(Year = years[i],alpha=talpha,beta=tbeta,
                       age3_ret = tescape*exp(talpha*(1-(tescape/tbeta)))) %>%
        mutate(exp_rate = ifelse(years[i]>=relYr,max((1-tar_esc/age3_ret),0.13),0.13),
               wild_escape = age3_ret*(1-exp_rate),
               hatchSmoltRel = sim.dat$HatchRelease[i],
               hatch_surv = ifelse(years[i]>=relYr,0.0675,0.015),
               hatch_return = hatchSmoltRel*hatch_surv,
               hatch_escape = hatch_return*(1-exp_rate),
               total_escape = wild_escape + hatch_escape)
    }  
    if(model=="BevHolt"){
      tdat<-data.frame(Year = years[i],alpha=talpha,beta=tbeta,
                       age3_ret = talpha*tescape/(tbeta+tescape)) %>%
        mutate(exp_rate = ifelse(years[i]>=relYr,(1-tar_esc/age3_ret),0.13),
               wild_escape = age3_ret*(1-exp_rate),
               hatchSmoltRel = sim.dat$HatchRelease[i],
               hatch_surv = ifelse(years[i]>=relYr,0.0675,0.015),
               hatch_return = hatchSmoltRel*hatch_surv,
               hatch_escape = hatch_return*(1-exp_rate),
               total_escape = wild_escape + hatch_escape)
    }
    tout.dat<-tout.dat %>% bind_rows(tdat)
  }
  return(tout.dat)
}
###### END STOCK RECRUIT MODEL FUNCTION #####


