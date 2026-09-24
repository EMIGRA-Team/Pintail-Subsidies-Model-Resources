# rm(list=ls()) # REMOVES ALL OBJECTS
library(session)     # install.packages("Johnson")    not available (for R version 3.4.1)   # library( rcompanion )  # detach("package:AICcmodavg", unload=TRUE)
wd="C:/Brady/Seafile/My Seafile Library/Programs/R/Pintail linkages"
setwd(wd)  # getwd()
#restore.session("pintail linkages.RData")   # stores all variables & loaded packages in this R file

library(easypackages) # install.packages("easypackages")  https://cran.r-project.org/web/packages/easypackages/README.html
packages('ggeffects', 'merTools')
#packages("AER", "AICcmodavg", "bestNormalize", "boot", "car", "cowplot", "directlabels", "effects", "emmeans", "ggplot2", "gmodels", "gridExtra",
#                "lme4", "lmtest", "multcomp", "MuMIn", "plyr", "quantmod", "rcompanion",
#                  "RCurl", "reshape2", "Rlab", "rlist", "Rmisc", "sandwich", "sjPlot", "stats", "visreg", "xlsx", "XML")   # detach("package:spacetime", unload=TRUE)

focal_years =2007:2016

# Zhao et al 2019: range of annual mean productivity estimates by substratum

zhao_prods = c(0.08,0.817)
zhao_age_ratios = zhao_prods/(1-zhao_prods)

# Import and clean scenarios from "Pintail Model Scenario Results Summary_AVG_WETNESS_Manuscript revision with SEs.xlsx"
scenarios_df0 = read.csv("Pintail Model Scenario Results Summary r1.csv", stringsAsFactors=F)  # head(scenarios_df0)
  names(scenarios_df0) = tolower(names(scenarios_df0))
scenarios_df0$scenario = tolower(scenarios_df0$scenario)     # nrow(scenarios_df0)   scenarios_df0[7,]
scenarios_df = scenarios_df0 #scenarios_df0[1:6, c(1,ncol(scenarios_df0)-1)]  # head(scenarios_df)

# Import and clean trend data by subregion
trends_df0 = read.csv("Pintail Pop Trend 1955-2018.csv") # head(trends_df0)
  names(trends_df0)  = tolower(names(trends_df0))
trends_df = trends_df0[,c(1,3,4,5)]  # head(trends_df)  str(trends_df) trends_df0$year %in% focal_years
  names(trends_df) = c("year","PPR_US","PPR_Canada","PPR_tot")
trends_df$prop_CN = with(trends_df,PPR_Canada/PPR_tot)
#trends_df$prop_US = with(trends_df,PPR_tot - 

#prop_CN_avg = mean(trends_df$prop_CN)

prop_CN_glmer = glmer(cbind(PPR_Canada, PPR_US)  ~ (1 | year ), family=binomial, data=trends_df) # summary(prop_CN_glmer) str(trends_df)

prop_CN_glm = glm(cbind(PPR_Canada, PPR_US)  ~ 1, family=binomial, data=trends_df) # summary(prop_CN_glmer) str(trends_df)

sfun1 <- function(x) {
  test = data.frame("year"=sample(trends_df$year,1))
  simulate(x,newdata=test,re.form=~0,
           allow.new.levels=TRUE)[[1]]
}
simulate(prop_CN_glm,newdata=test,
         allow.new.levels=TRUE)[[1]]
################################################ Note: this approach yields negative predicted counts (!), perhaps due to the large random effect.
################################################ Scroll down for alternative approach

#simulate(prop_CN_glmer,newdata=test,re.form=NULL,
#         allow.new.levels=TRUE)[[1]]

#mean(trends_df$PPR_Canada)
## param, RE, and conditional
b1 <- bootMer(prop_CN_glmer,FUN=sfun1,nsim=100,seed=101) # dim(b1$t) summary(prop_CN_glmer)
b1_CN = b1$t[row.names(b1$t)=="PPR_Canada",] # summary(b1_CN)
b1_US = b1$t[row.names(b1$t)=="PPR_US",] # summary(b1_US) head(b1_US)
b1_US[b1_US<0] = 0
b1_p = b1_CN / (b1_CN + b1_US)
b1_pred_interval = c(mean(b1_p),quantile(b1_p,c(0.025,0.975)))  ### generates CI for manuscript

## predictInterval method -- used for the manuscript
predint_prop_CN_glmer = predictInterval(prop_CN_glmer, newdata = data.frame(year=trends_df$year), type = 'probability') # gives expectation & CI by year
prop_CN_glmer_preds0 = colMeans(predint_prop_CN_glmer)
prop_CN_glmer_preds = data.frame(t(prop_CN_glmer_preds0[c(3,1,2)]))
  names(prop_CN_glmer_preds) = c("lcl", "avg", "ucl")

## standard population-level prediction
#pframe <- data.frame(test,pred=predict(prop_CN_glmer,newdata=test,re.form=NA, type = "response"))

################################################ Brute force method to calculating CI for prop_CN posted by Ben Bolker
################################################ https://stackoverflow.com/questions/35235939/how-to-plot-logistic-glm-predicted-values-and-confidence-interval-in-r

newdata_=NULL
linkinv <- family(prop_CN_glm)$linkinv ## inverse-link function
prop_CN_glm_preds0 <- predict(prop_CN_glm,newdata=newdata_,se.fit=TRUE)
alpha <- 0.95; sc <- abs(qnorm((1-alpha)/2))  ## Normal approx. to likelihood
prop_CN_glm_preds <- c(linkinv(prop_CN_glm_preds0$fit - sc*prop_CN_glm_preds0$se.fit)[1],
                       linkinv(prop_CN_glm_preds0$fit)[1],
                       linkinv(prop_CN_glm_preds0$fit + sc*prop_CN_glm_preds0$se.fit)[1])  #### SE is too small, so CI is 0




#clutch_size = 6.9 # global mean from Duncan 1987 - southeast of Kitsim in southern Alberta; data 1981 to 1984
#ducklg_surv_Guyn = c(0.42, 0.44) # annual means Guyn & Clark 1999 - same study area as Duncan 1987;  data 1995 to 1996; had higher estimate from 1994 ()0.65) but this was a wet year and not representative
    # Devries wrote: New [& unpublished] research on pintail duckling survival in prairie Canada also shows pintail DS to be in the 0.35-0.40 range

# Calculate recruitment per scenario
  # Use hatched nests ? apply clutch size ? apply 50:50 sex ratio - apply duckling survival to fledge (about 40%). This number is divided by (the spring female population * breeding season survival) = fall adults females
#female_male_ratio = 0.5
#scenarios_df$recruit_rate_CN =  scenarios_df$hen_success*clutch_size*female_male_ratio*mean(ducklg_surv_Guyn)    # head(scenarios_df)
  names(scenarios_df)[names(scenarios_df) == 'fall_fem_ar']  = 'recruit_rate_CN'
 # assume the status-quo recruitment for Canada = status-quo recruitment for US, and
   # then apply the latter value to the US for the remaining scenarios
scenarios_df$recruit_rate_US = scenarios_df$recruit_rate_CN[2]    # head(scenarios_df)
scenarios_df$recruit_rate_avg = scenarios_df$recruit_rate_CN #* prop_CN_glmer_preds$avg + scenarios_df$recruit_rate_US * (1-prop_CN_glmer_preds$avg)
scenarios_df$recruit_rate_lcl = scenarios_df$recruit_rate_avg  - 1.96*scenarios_df$fall_fem_ar_se  #* prop_CN_glmer_preds$lcl + scenarios_df$recruit_rate_US * (1-prop_CN_glmer_preds$lcl)
scenarios_df$recruit_rate_ucl = scenarios_df$recruit_rate_avg + 1.96*scenarios_df$fall_fem_ar_se  #* prop_CN_glmer_preds$ucl + scenarios_df$recruit_rate_US * (1-prop_CN_glmer_preds$ucl) 


#save.session("pintail linkages.RData")   # stores all variables & loaded packages in this R file