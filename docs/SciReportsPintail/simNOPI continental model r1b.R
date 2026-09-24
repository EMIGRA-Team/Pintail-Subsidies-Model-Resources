# rm(list=ls()) # REMOVES ALL OBJECTS
#source('~/.active-rstudio-document', print.eval = F)
library(session)     # install.packages("Johnson")    not available (for R version 3.4.1)   # library( rcompanion )  # detach("package:AICcmodavg", unload=TRUE)
wd="C:/Brady/Seafile/My Seafile Library/Programs/R/Pintail linkages" # getwd()
setwd(wd)  # getwd()
restore.session("pintail linkages.RData")   # stores all variables & loaded packages in this R file
# detach("package:xlsx", unload=TRUE)
library(easypackages) # install.packages("plotrix")  https://cran.r-project.org/web/packages/easypackages/README.html
packages("abind", "boot", "forcats", "ggplot2", "ggrepel", "openxlsx",  "plotrix", "plyr", "purrr", "reshape", "reshape2", prompt=F)   # "openxlsx","xlsx" "dataframes2xls", 

# Can skip/ignore ####

############### Close graph windows
while (.Device=="windows"){dev.off()}     

############### Digits after decimal function for |x|<10
num_digits <- function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])-1

## Source external code ####

##### Primary user-assigned parameter values
source("inpNOPI continental model.r")

##### Recruitment scenarios for linkages paper
source("scnNOPI recruitment PPR r1.r")

### Frequently changed input values ####

k_incs = 20                     # Number of increments for harvest levels in simulation
k_max = 0.2                     # Maximum harvest rate   
#k_plot = 0.1052632 # 0.0947 # Harvest level at which to plot population trajectory ; corresponds with kill rate corresponding with a "moderate" harvest rate (including crippling loss) of 0.08 (Fig 6 in Mattsson et al. 2012)

dummy = 0

#### Prepare mini data frame of R2 based on pct spring-seeded crop converted to grass; Fig 4 ####
# input values are from "Pintail Model Scenario Results Summary_AVG_WETNESS_Manuscript revision with SEs.xlsx"
tot_SScrop_ac_PPR = 61825491
cost_per_ac_SScrop_idle_grass = 801 # see scenarios tab in "Tables linkages ms.xlsx"

R2_crazy_df = data.frame(pct_crop_to_grass = c(10,15,20))
R2_crazy_df$avg = c(0.583505799, 0.624738424, 0.665618714)
R2_crazy_df$SE = c(0.0559, 0.0599, 0.0637)

R2_crazy_avg_lm = lm(avg ~ 1 + pct_crop_to_grass, R2_crazy_df) # 
R2_crazy_avg_lm_coeffs =  coefficients(R2_crazy_avg_lm)

R2_crazy_SE_lm = lm(SE ~ 1 + avg, R2_crazy_df) # 
R2_crazy_SE_lm_coeffs =  coefficients(R2_crazy_SE_lm)

pct_crop_to_grass = seq(0,100, by = 10) # c(0,100)
R2_crazy_df2 = data.frame(pct_crop_to_grass) #  conversion from spring-seeded crop to idle grass
R2_crazy_df2$avg = R2_crazy_avg_lm_coeffs[2]*pct_crop_to_grass + R2_crazy_avg_lm_coeffs[1]  # 0 to 100% converted
R2_crazy_df2$SE = R2_crazy_SE_lm_coeffs[2]*R2_crazy_df2$avg + R2_crazy_SE_lm_coeffs[1]
R2_crazy_df2$lcl = with(R2_crazy_df2, avg - 1.96*SE)
R2_crazy_df2$ucl = with(R2_crazy_df2, avg + 1.96*SE) # head(R2_crazy_df2)
R2_crazy_df3 = R2_crazy_df2[, c(4,2,5)] # ncol(R2_plot_df); head(R2_plot_mat)
rownames(R2_crazy_df3) = paste(pct_crop_to_grass,"pct_crop_to_grass", sep="_") # R2_0s %in% R2_plot_mat

##### Create change_params_df for sensitivity analysis ####

#recruit_rates_df = scenarios_dfscenarios_concise_df
Sb2ms= c(0.89, 0.96, 0.99) # Sb2m; breeding-season survival rate for males based on 76 of 79 males survived in Brasher et al. 2006; Agresti-Coull 95% confidence interval
hs_empirical_plot = c(0, 0.05, 0.09, 0.15) #  predictions from mixed logistic regression of annual pintail harvest rates during years when bag limit was 1; see "Runge & Boomer 2005 table appendix C.xlsx"
hs_plot = c(0.034, 0.067, 0.101) # these are the baseline (adult female) harvest rates that correspond with simulated harvest rates that match the empirical mean & CIs
scenarios_concise_df = scenarios_df[,c("recruit_rate_lcl", "recruit_rate_avg", "recruit_rate_ucl")]
  rownames(scenarios_concise_df) = scenarios_df$scenario
  names(scenarios_concise_df) = names(prop_CN_glmer_preds)
  
R2_scenarios_all_df = rbind(scenarios_concise_df, R2_crazy_df3)

change_params_df = data.frame(rbind(R2_scenarios_all_df, prop_CNs=prop_CN_glmer_preds, Sb2ms=Sb2ms, hs_plot=hs_plot))
#  names(change_params_df) = c('lcl', 'mean', 'ucl')

# annual harvest rate averaged 0.09 (95% confidence interval: 0.05, 0.15

###### DD params (min_intercept,max_intercept; slope) ####

#R2_0_min=0                                     # 0.06
#R2_0_max=round(R2_0_min+log(1.1),2)                                   # .15
#R2_0_incs=2
#R2_0s=rev(seq(R2_0_min,R2_0_max,length=R2_0_incs))
#R2_0s= scenarios_df$recruit_rate_avg #seq(R2_0_min,R2_0_max,length=R2_0_incs) # for linkages paper
#R2_tests = c(0.64, 1.0)  # range of values according to Mattsson et al 2012
#R2_0s= R2_tests
#R2_0s= scenarios_df$recruit_rate_avg
zhao_age_ratios_seq = seq(min(zhao_age_ratios), max(zhao_age_ratios), length.out = 10)
#R2_0s= with(scenarios_df, sort(c(recruit_rate_lcl, recruit_rate_avg, recruit_rate_ucl, seq(0.6,2.0,length.out=10)))) # length(R2_0s)

#R2_B2_min=-0.12                                    # -0.24 -.08
#R2_B2_max=-0.2                                    # -0.2       ; -.239 leads to no real difference in sensitivity btwn PR & GC
#R2_B2_incs=R2_0_incs
R2_B2s=rep(R2_B2,R2_0_incs)*1e-6



####### Define other constants and initial values for simulation ####
t.max=100                 # t.max=2 will run for one year
years=1:t.max                             

  # Initialize vectors for breeding grounds in AK
  R1 =    mat.or.vec(t.max, 1) # Recruitment
  B1am =   mat.or.vec(t.max, 1) # Breeding population size for adult males
  B1af =   mat.or.vec(t.max, 1) # Breeding population size for adult females
  B1amf =  mat.or.vec(t.max, 1) # Combined adult male and adult female breeding population size
  B1jm =   mat.or.vec(t.max, 1) # Number of male juveniles at end of breeding season
  B1jf =   mat.or.vec(t.max, 1) # Number of female juveniles at end of breeding season

  # Initialize vectors for breeding grounds in PR
  R2 =    mat.or.vec(t.max, 1) # Recruitment
  B2am =   mat.or.vec(t.max, 1) # Breeding population size for adult males
  B2af =   mat.or.vec(t.max, 1) # Breeding population size for adult females
  B2amf =  mat.or.vec(t.max, 1) # Combined adult male and adult female breeding population size

  B2jm =   mat.or.vec(t.max, 1) # Number of male juveniles at end of breeding season
  B2jf =   mat.or.vec(t.max, 1) # Number of female juveniles at end of breeding season
  
  R2_0s_post_hoc = seq(0.6,2.0,length.out=10)
  

  #ks = rev(seq(0,k_max,length=k_incs))                                              # Vector for examining influence of harvest rate
  ks0 =  seq(0,k_max,length=k_incs)  # k_plot                                             # Vector for examining influence of harvest rate
  ks = sort(c(ks0, as.numeric(change_params_df["hs_plot",] / kill_keep)))
  

###***** Define simulation function ####
  level_ = 3
  append_level_ = 3
simNOPI = function(level_){
  ############### Reproduction in PPR ####
  
  R2_0s= change_params_df[1:nrow(R2_scenarios_all_df),level_]
  #if(level_==append_level_){R2_0s=c(R2_0s,R2_0s_post_hoc)}
  R2_correctn =  R2_0s[2] / exp(R2_0_min)   #  rescales age ratios in other breeding regions to age ratio under Status Quo for PPR
  
  
  ############### Breeding survival ####
    # AK
    Sb1m = Sbm    
    Sb1f = Sbf    
           
    # PR
    Sb2m = change_params_df["Sb2ms", level_] # Sbm    
    Sb2f = Sbf    

    # NU
    Sb3m = Sbm    
    Sb3f = Sbf  

  # Initialize vectors for breeding grounds in AK+NU
  R3 =    mat.or.vec(t.max, 1) # Recruitment
  B3am =   mat.or.vec(t.max, 1) # Breeding population size for adult males
  B3af =   mat.or.vec(t.max, 1) # Breeding population size for adult females
  B3amf =  mat.or.vec(t.max, 1) # Combined adult male and female breeding population size

  B3jm =   mat.or.vec(t.max, 1) # Number of male juveniles at end of breeding season
  B3jf =   mat.or.vec(t.max, 1) # Number of female juveniles at end of breeding season
  
# Breeding grounds summary
  Bamf =  mat.or.vec(t.max, 1);# Combined adult male and adult female breeding population size

############### Fall migration ####
    
  # Survival probabilities
    # Adults
      # Males
        Sbw11am = Sbw11a # From AK+NU to CA
        Sbw22am = Sbw22a # From PR to GC
        Sbw12am = Sbw12a # From AK+NU to GC
        Sbw21am = Sbw21a # From PR to CA
        Sbw32am = Sbw32a # From NU to GC
        Sbw31am = Sbw31a # From NU to CA

      # Females
        Sbw11af = Sbw11a # From AK+NU to CA
        Sbw22af = Sbw22a # From PR to GC
        Sbw12af = Sbw12a # From AK+NU to GC
        Sbw21af = Sbw21a # From PR to CA
        Sbw32af = Sbw32a # From NU to GC
        Sbw31af = Sbw31a # From NU to CA

    # Juveniles
      # Males
        Sbw11jm = Sbw11j # From AK+NU to CA
        Sbw22jm = Sbw22j # From PR to GC
        Sbw12jm = Sbw12j # From AK+NU to GC
        Sbw21jm = Sbw21j # From PR to CA
        Sbw32jm = Sbw32j # From NU to GC
        Sbw31jm = Sbw31j # From NU to CA

      # Females
        Sbw11jf = Sbw11j # From AK+NU to CA
        Sbw22jf = Sbw22j # From PR to GC
        Sbw12jf = Sbw12j # From AK+NU to GC
        Sbw21jf = Sbw21j # From PR to CA
        Sbw32jf = Sbw32j # From NU to GC
        Sbw31jf = Sbw31j # From NU to CA
        
  # Harvest 
    
    Htot=mat.or.vec(t.max, 1)   # Annual number of pintails (adults and juveniles, males and females) harvested  
  
############### Parameters for wintering grounds in CA ####
Sw1_min = Sw_min
Sw1_max = Sw_max    # Maximum natural survival probability (excluding harvest) during winter and spring -- Fleskes et al report a maximum of 0.99 for 1 - natural winter mortality, but this doesn't include mortality during spring...

  # Density dependence and environmental effects on survival
    Sw1W1mf  = SwWmf # Combined male & female winter population size  -15e-9 
    
  # Initialize vectors
    W1m = mat.or.vec(t.max, 1);                             # Winter population size for males
    W1f = mat.or.vec(t.max, 1);                            # Winter population size for females
    W1mf = mat.or.vec(t.max, 1);                                                # Winter population size for males & females
    Sw1 = mat.or.vec(t.max, 1); Sw1[1] = Sw1_max                                # Winter survival rate for males & females
    
############### Parameters for wintering grounds in GC
Sw2_min = Sw_min
Sw2_max = Sw_max    # Maximum survival probability during winter

  # Density dependence and environmental effects on survival
    Sw2W2mf  = SwWmf # Combined male & female winter population size    -15e-9

  # Initialize vectors
    W2m = mat.or.vec(t.max, 1);                             # Winter population size for males 
    W2f = mat.or.vec(t.max, 1);                               # Winter population size for females
    W2mf= mat.or.vec(t.max, 1) # Winter population size for males & females
    Sw2 = mat.or.vec(t.max, 1); Sw2[1] = Sw2_max                          # Winter survival rate for males & females
    
# Summarize wintering grounds
    Wmf= mat.or.vec(t.max, 1); Wmf[1] = W1m[1] + W1f[1] + W2m[1] + W2f[1]       # Initial vector for winter population size for males & females

############### Spring migration probabilities ####
    n12 = 1-n11


############# Initialize results vectors across initial values and constants ####
  Bmf0_eq      =               # Number of individuals living at start of final breeding season (equilibrium population size)
  B1mf0_eq      =               # Number of individuals living at start of final breeding season in AK (equilibrium population size)
  B2mf0_eq      =               # Number of individuals living at start of final breeding season in PR (equilibrium population size)
  B3mf0_eq      =               # Number of individuals living at start of final breeding season in NU (equilibrium population size)
      
  Rm_eq       =                # Male age ratio across regions
  Rf_eq       =                # Female age ratio across regions

  Bmf_eq      =                # Number of individuals living at end of final breeding season (equilibrium population size)
  h_prop_eq      =                # Proportion of all pintails harvested in final year

  cum_harv    =                # Cumulative number harvested across years
  Harv_sust   =                # Number harvested in final year (equilibrium or sustained yield)

  W1mf0_eq      =               # Number of individuals living at end of final hunting season in CA (equilibrium population size)
  W2mf0_eq      =               # Number of individuals living at end of final hunting season in GC (equilibrium population size)
  Wmf_eq      = array(NA, dim=c(length(ks), length(dummy), length(R2_0s)))     # Number of individuals living at end of final winter (equilibrium population size)
    
############# Initialize arrays to store results for each combination of year and initial values
      
  # Trajectories for each level of harvest and GC winter survival

  B1f0s       =                # Number of females at beginning of breeding season in AK+NU
  B1mf0s       =               # Number of males & females at beginning of breeding season in AK+NU

  B2f0s       =                # Number of females at beginning of breeding season in PR
  B2mf0s       =                 # Number of males & females at beginning of breeding season in PR

  Bmf0s        =                   # Number of males and females at start of breeding season across regions

  Bm0_Bf0_s    =                # Sex ratio at beginning of breeding season
  Bm_Bf_s    =                # Sex ratio at end of breeding season

  R1s         =                # Age ratio at end of breeding season in AK+NU
  R2s         =                # Age ratio at end of breeding season in PR
  Rs          =                # Continental age ratio at end of breeding season

  Sw1s        =                # Winter survival in CA
  Sws         =                # Continental winter-spring survival

  W1mf0s      =                # Number of males and females at start of winter in CA

  W2mf0s      =                  # Number of males and females at start of winter in GC    

  Wmf0s        =                 #  Continental post-harvest population size

  h_avgs       =                 # Average harvest rate across cohorts

  S11afs       =                 # Adult female survival AK to CA
  S11ams       =                 # Adult male survival AK to CA
  S11jfs       =                 # Juvenile female survival AK to CA
  S11jms       =                 # Juvenile male survival AK to CA
  
  array(NA,dim=c(t.max, length(ks), length(dummy), length(R2_0s)))               # Specify array dimensions
############################################################## Begin Loops #############################################################################

############# While loop to examine effects of increasing GC winter survival
for (d in dummy){  # placeholder for pintail linkages paper
  #print(c("##############Sw2_miax=",Sw2_max))
  #print(Wmf_eq_)

  
 for (R2_0 in R2_0s){ # R2_0s = list of values from scenarios for pintail linkages paper
   # Density dependence for age ratio in PR
   R2_B2 = R2_B2s[match(R2_0,R2_0s)]  # Assume no intraregional DD in PPR for linkages paper

  k = ks[1]  
    ############# For loop to examine effects of increasing harvest rate
  for (k in ks) {

    # Increment harvest rates relative to adult females 
    # Adults
      # Males
        k11am = k*Vam # From AK+NU to CA
        k22am = k*Vam # From PR to GC
        k12am = k*Vam # From AK+NU to GC
        k21am = k*Vam # From PR to CA

      # Females
        k11af = k*Vaf # From AK+NU to CA
        k22af = k*Vaf # From PR to GC
        k12af = k*Vaf # From AK+NU to GC
        k21af = k*Vaf # From PR to CA

    # Juveniles
      # Males
        if (k*Vjm > 1) {k11jm=k22jm=k12jm=k21jm=1} else {k11jm=k22jm=k12jm=k21jm=k*Vjm};

      # Females
        k11jf = k*Vjf # From AK+NU to CA
        k22jf = k*Vjf # From PR to GC
        k12jf = k*Vjf # From AK+NU to GC
        k21jf = k*Vjf # From PR to CA   

    # Initial breeding population size for males & females based on USFWS 2009 waterfowl status report         
    B1f0 = B1m0 = 0.5*p_B1_vs_B2andB3*B0
    B2f0 = B2m0 = 0.5*(1-p_B1_vs_B2andB3)*B0*p_B2_vs_B3
    B3f0 = B3m0 = 0.5*(1-p_B1_vs_B2andB3)*B0*(1-p_B2_vs_B3)         
    Bm0=B1m0+B2m0+B3m0
    Bf0=B1f0+B2f0+B3f0
    Bmf00 = Bmf0 = Bm0 + Bf0
    
    t=2
    
    ############### Metapopulation simulation beginning with initial year in spring ####
    while (t < t.max+1 & Bm0>=1 & Bf0>=1) { 
      # Breeding grounds in AK continued from bottom of code
        # Breeding population size at beginning of breeding season
        B1mf0   = B1m0 + B1f0    # Males & females

        B1f0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B1f0                     # Number of females at beginning of breeding season
        B1mf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B1mf0                   # Number of males & females at beginning of breeding season        
  
        # Reproduction in AK   ####
        R1[t] =  exp(R1_0) * R2_correctn #exp(R1_0 + R1_B1*B1mf0 ) #
                                    
        R1s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = R1[t]                          # Age ratio at end of breeding season
        
        # Adult population size at end of breeding season in AK      
        B1am[t] = Sb1m * B1m0  # Males
        B1af[t] = Sb1f * B1f0  # Females
        B1amf[t] = B1am[t] + B1af[t]                                            # Males & females

        B1jm[t] = B1jf[t] = B1af[t]*R1[t]                                      # Juveniles by sex, assumes 50:50 sex ratio -- female survival is subsumed by age ratio        
    
      # Breeding grounds in PR continued from bottom of code
        # Breeding population size at beginning of breeding season in PR
        B2mf0   = B2m0 + B2f0    # Males & females
        
        B2f0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B2f0                     # Number of females at beginning of breeding season
        B2mf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B2mf0                   # Number of males & females at beginning of breeding season        
          
        # Reproduction in PR # CONSTRUCTION ####
        #R2_succ =inv.logit(R2_0 + R2B2mf*B2mf0 + R2_lat*B2_lat )
        #R2[t] = exp(R2_0 + R2_B2*B2mf0 + R2_P*P ) 
        R2[t] = R2_0  # Assume no intraregional DD in PPR for linkages paper, here R2_0 represents actual recruitment instead of intercept of DD relationship                                 
        
        R2s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = R2[t]               # Age ratio at end of breeding season
  
        # Adult population size at end of breeding season in PR
        B2am[t] = Sb2m * B2m0    # Males
        B2af[t]= Sb2f * B2f0     # Females
        B2amf[t] = B2am[t] + B2af[t]                                              # Males & females

        B2jm[t] = B2jf[t] = B2af[t]*R2[t]                                         # Juveniles by sex, assumes 50:50 sex ratio -- female survival is subsumed by age ratio                         

      # Breeding grounds in NU continued from bottom of code
        # Breeding population size at beginning of breeding season
        B3mf0   = B3m0 + B3f0    # Males & females

        #B3f0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B3f0                     # Number of females at beginning of breeding season
        #B3mf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B3mf0                   # Number of males & females at beginning of breeding season        
  
        # Reproduction in NU    ####           
        R3[t] =  exp(R3_0) * R2_correctn #exp(R3_0 + R3_B3*B3mf0 ) # 
                                    
        #R3s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = R3[t]                          # Age ratio at end of breeding season
        
        # Adult population size at end of breeding season in AK+NU      
        B3am[t] = Sb3m * B3m0  # Males
        B3af[t] = Sb3f * B3f0  # Females
        B3amf[t] = B3am[t] + B3af[t]                                            # Males & females

        B3jm[t] = B3jf[t] = B3af[t]*R3[t]                                      # Juveniles by sex, assumes 50:50 sex ratio -- female survival is subsumed by age ratio        

    
      # Breeding grounds summary
        Bmf0 = B1m0 + B1f0 + B2m0 + B2f0 + B3m0 + B3f0                                       # Number of males and females at beginning of breeding season (BPOP)
        Bmf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Bmf0                     # Number of males & females at beginning of breeding season      }  
          # Bmf0s[t,       ,              ,                 ]

        Bm0 = B1m0 + B2m0 + B3m0 

        Bf0 = B1f0 + B2f0 + B3f0         

        Bm0_Bf0_s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Bm0/Bf0                     # Number of males & females at beginning of breeding season        

        Bm =  B1am[t] + B1jm[t] + 
                B2am[t] + B2jm[t] +      
                B3am[t] + B3jm[t] 
                
        Bf =  B1af[t] + B1jf[t] + 
                B2af[t] + B2jf[t] +      
                B3af[t] + B3jf[t]         
        
        Bmf =   B1am[t] + B1jm[t] + B1af[t] + B1jf[t] + 
                B2am[t] + B2jm[t] + B2af[t] + B2jf[t] +      
                B3am[t] + B3jm[t] + B3af[t] + B3jf[t]                                     # Number of males and females at end of breeding season (pre-harvest)
 
        Bm_Bf_s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Bm/Bf 

        #Rm = (B1am[t] + B2am[t] + B3am[t]) / (B1jm[t] + B2jm[t] + B3jm[t])                          # Male age ratio across regions
        R = (B1jf[t] + B2jf[t] + B3jf[t]) / (B1af[t] + B2af[t] + B3af[t])                            # Continental female age ratio 
        Rs[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = R               # Continental age ratio at end of breeding season


      # Population size just before harvest (just after fall survival)
          Fam = B1am[t]*(m11*Sbw11am+(1-m11)*Sbw12am) + 
                  (B2am[t]+B3am[t])*(m22*Sbw22am+(1-m22)*Sbw21am)
                  
          Faf = B1af[t]*(m11*Sbw11af+(1-m11)*Sbw12af) + 
                  (B2af[t]+B3af[t])*(m22*Sbw22af+(1-m22)*Sbw21af)  
                  
          Fjm = B1jm[t]*(m11*Sbw11jm+(1-m11)*Sbw12jm) + 
                  (B2jm[t]+B3jm[t])*(m22*Sbw22jm+(1-m22)*Sbw21jm)   
                  
          Fjf = B1jf[t]*(m11*Sbw11jf+(1-m11)*Sbw12jf) + 
                  (B2jf[t]+B3jf[t])*(m22*Sbw22jf+(1-m22)*Sbw21jf)  
                  
          Ftot = Fam + Faf + Fjm + Fjf                 
        
      # Fall harvest ####
        # Adults
          # Males
          Ham = (B1am[t]*(m11*Sbw11am*k11am+(1-m11)*Sbw12am*k12am) + 
                  (B2am[t]+B3am[t])*(m22*Sbw22am*k22am+(1-m22)*Sbw21am*k21am))*kill_keep                                 
          
          # Females        
          Haf = (B1af[t]*(m11*Sbw11af*k11af+(1-m11)*Sbw12af*k12af) + 
                  (B2af[t]+B3af[t])*(m22*Sbw22af*k22af+(1-m22)*Sbw21af*k21af))*kill_keep  
        
        # Juveniles
          # Males
          Hjm = (B1jm[t]*(m11*Sbw11jm*k11jm+(1-m11)*Sbw12jm*k12jm) + 
                  (B2jm[t]+B3jm[t])*(m22*Sbw22jm*k22jm+(1-m22)*Sbw21jm*k21jm))*kill_keep   
         
          # Females        
          Hjf = (B1jf[t]*(m11*Sbw11jf*k11jf+(1-m11)*Sbw12jf*k12jf) + 
                  (B2jf[t]+B3jf[t])*(m22*Sbw22jf*k22jf+(1-m22)*Sbw21jf*k21jf))*kill_keep   
                    
        Htot[t] = Ham + Haf + Hjm + Hjf                                           # Males and females, adults and juveniles


      # Wintering grounds in CA
        # Population size at beginning of winter
        W1m0 = B1am[t]*m11*Sbw11am*(1-k11am) + B1jm[t]*m11*Sbw11jm*(1-k11jm) + 
                (B2am[t]+B3am[t])*(1-m22)*Sbw21am*(1-k21am) + (B2jm[t]+B3jm[t])*(1-m22)*Sbw21jm*(1-k21jm)                
               
        W1f0 = B1af[t]*m11*Sbw11af*(1-k11af) + B1jf[t]*m11*Sbw11jf*(1-k11jf) + 
                (B2af[t]+B3af[t])*(1-m22)*Sbw21af*(1-k21af) + (B2jf[t]+B3jf[t])*(1-m22)*Sbw21jf*(1-k21jf)                
                  
        W1mf0 = W1m0 + W1f0      # Males & females
        W1mf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = W1mf0                     # Males & females       
        
      
        # Survival
        Sw1_succ     = inv.logit(Sw1_0 + Sw1W1mf*W1mf0)

        Sw1[t] = Sw1_min*(1-Sw1_succ) + Sw1_max*Sw1_succ
        
        Sw1s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Sw1[t]              
    
        # Population size at end of winter
        W1m[t]  = W1m0*Sw1[t]    # Males
        W1f[t]  = W1f0*Sw1[t]    # Females
        W1mf[t]   = W1m[t] + W1f[t]                                               # Males & females
    
      # Wintering grounds in GC                     
        # Population size at beginning of winter
        W2m0 =  B1am[t]*(1-m11)*Sbw12am*(1-k12am) + B1jm[t]*(1-m11)*Sbw12jm*(1-k12jm) +
                (B2am[t]+B3am[t])*m22*Sbw22am*(1-k22am) + (B2jm[t]+B3jm[t])* m22*Sbw22jm*(1-k22jm)
               
        W2f0 =  B1af[t]*(1-m11)*Sbw12af*(1-k12af) + B1jf[t]*(1-m11)*Sbw12jf*(1-k12jf) +
                (B2af[t]+B3af[t])*m22*Sbw22af*(1-k22af) + (B2jf[t]+B3jf[t])* m22*Sbw22jf*(1-k22jf)   
                            
        W2mf0 = W2m0 + W2f0      # Males & females

        W2mf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = W2mf0                    # Males & females
      
        # Survival
        Sw2_succ     = inv.logit(Sw2_0 + Sw2W2mf*W2mf0)

        Sw2[t] = Sw2_min*(1-Sw2_succ) + Sw2_max*Sw2_succ
        
        # Population size at end of winter
        W2m[t]  = W2m0*Sw2[t]  # Males
        W2f[t]  = W2f0*Sw2[t]  # Females
        W2mf[t]   = W2m[t] + W2f[t]                                             # Males & females
      
      # Wintering grounds summary   
        Wmf0 = W1mf0 + W2mf0
        Wmf =  W1mf[t] + W2mf[t]                  # Continental BPOP      
        Wmf0s[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = W1mf0 + W2mf0                    # Continental post-harvest population size   
        Sw = Wmf / (W1mf0 + W2mf0)                   # Continental winter-spring survival
        Sws[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Sw                              # Vector for continental winter-spring survival
        
      # Average harvest rate across cohorts
        h_avg = Htot[t]/Ftot
        h_avgs[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = h_avg                              # Vector for continental winter-spring survival 

      # Spring migration
        #n11 = n11_max*inv.logit(n11_0 + n11Wmf*Wmf[t])                        # Probability of migrating from CA to AK+NU vs. CA to PR
        #n22 = n22_max*inv.logit(n22_0 + n22Wmf*Wmf[t] + n22B2_lat*B2_lat)     # Probability of migrating from GC to PR vs. GC to AK+NU

        N_arrive2 = W1mf0*n12* Sw1[t] + W2mf0*n22* Sw2[t]
        psi_PRdprt = psi_PRdprt_max*inv.logit(psi_PRdprt_0 + psi_PRdprt_N_arrive2*N_arrive2 + psi_PRdprt_P*P)     # Probability that PR arrivals leave and migrate to AK or NU
          
      # Breeding grounds in AK continued above
        # Breeding population size at beginning of breeding season in AK
        B1m0    = (W1m0 * n11 * Sw1[t]  + W2m0 * (1-n22) * Sw2[t]) + ((W1m0 * n12 * Sw1[t]  + W2m0 * Sw2[t] * n22) * psi_PRdprt * (1-psi_PRtoNU))                             # Males
        B1f0    = (W1f0 * n11 * Sw1[t]  + W2f0 * (1-n22) * Sw2[t]) + ((W1f0 * n12 * Sw1[t]  + W2f0 * Sw2[t] * n22) * psi_PRdprt * (1-psi_PRtoNU))                             # Females    

      # Breeding grounds in PR continued above
        # Breeding population size at beginning of breeding season in PR
        B2m0    = (W1m0 * n12 * Sw1[t]  + W2m0 * Sw2[t] * n22) * (1-psi_PRdprt)                        # Adult males
        B2f0    = (W1f0 * n12 * Sw1[t]  + W2f0 * Sw2[t] * n22) * (1-psi_PRdprt)                        # Adult females    

      # Breeding grounds in NU continued above
        # Breeding population size at beginning of breeding season in NU
        B3m0    =  (W1m0 * n12 * Sw1[t]  + W2m0 * Sw2[t] * n22) * psi_PRdprt * psi_PRtoNU                        # Adult males
        B3f0    =  (W1f0 * n12 * Sw1[t]  + W2f0 * Sw2[t] * n22) * psi_PRdprt * psi_PRtoNU                        # Adult females    

        Bmf0 = B1m0 + B1f0 + B2m0 + B2f0 + B3m0 + B3f0
           
      # Annual survival
        # Adults
          # Males
          S11am = Sb1m * Sbw11am * (1-k11am) * Sw1[t]                               # AK+NU to CA
          S12am = Sb1m * Sbw12am * (1-k12am) * Sw2[t]          
          S21am = Sb2m * Sbw21am * (1-k21am) * Sw1[t]
          S22am = Sb2m * Sbw22am * (1-k22am) * Sw2[t]
          
          # Females
          S11af = Sb1f * Sbw11af * (1-k11af) * Sw1[t]                           # AK+NU to CA
          S12af = Sb1f * Sbw12af * (1-k12af) * Sw2[t]                           # AK+NU to GC
          S21af = Sb2f * Sbw21af * (1-k21af) * Sw1[t]                           # PR to CA
          S22af = Sb2f * Sbw22af * (1-k22af) * Sw2[t]                           # PR to GC
   
       
        # Juveniles
          # Males
          S11jm = Sb1m * Sbw11jm * (1-k11jm) * Sw1[t]                           # AK+NU to CA
          S12jm = Sb1m * Sbw12jm * (1-k12jm) * Sw2[t]                           # AK+NU to GC
          S21jm = Sb2m * Sbw21jm * (1-k21jm) * Sw1[t]                           # PR to CA
          S22jm = Sb2m * Sbw22jm * (1-k22jm) * Sw2[t]                           # PR to GC
          
          # Females
          S11jf = Sb1f * Sbw11jf * (1-k11jf) * Sw1[t]                           # AK+NU to CA
          S12jf = Sb1f * Sbw12jf * (1-k12jf) * Sw2[t]                           # AK+NU to GC
          S21jf = Sb2f * Sbw21jf * (1-k21jf) * Sw1[t]                           # PR to CA
          S22jf = Sb2f * Sbw22jf * (1-k22jf) * Sw2[t]                           # PR to GC

          S11afs[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = S11af
          S11ams[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = S11am
          S11jfs[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = S11jf          
          S11jms[t,match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = S11jm          
    
      t=t+1
      } # End while loop with final year in winter
      
      # Store results for each level of harvest and GC winter survival ####
      Bmf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Bmf0                         # Number of individuals living at start of final breeding season (equilibrium population size)
      B1mf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B1mf0                       # Number of individuals living at start of final breeding season in AK (equilibrium population size)
      B2mf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B2mf0                       # Number of individuals living at start of final breeding season in AK (equilibrium population size)
      B3mf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = B3mf0                       # Number of individuals living at start of final breeding season in AK (equilibrium population size)

      Rm_eq = (B1am[t-1] + B2am[t-1]) / (B1jm[t] + B2jm[t-1])                          # Male age ratio across regions
      Rf_eq = (B1af[t-1] + B2af[t-1]) / (B1jf[t] + B2jf[t-1])                          # Female age ratio across regions        
      Bmf_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Bmf                         # Number of individuals living at end of final breeding season (equilibrium population size)

      cum_harv[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = sum(Htot)                 # Cumulative number harvested across years
      Harv_sust[match(k,ks),match(d,dummy),match(R2_0,R2_0s)]=Htot[t-1]                  # Number harvested in final year (equilibrium or sustained yield)
      h_prop_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Htot[t-1]/Ftot           # Proportion of all pintails harvested in final year           
      
      
      Wmf_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = Wmf[t-1]                    # Number of individuals living at end of final winter (equilibrium population size)
      W1mf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = W1mf0                       # Number of individuals living at end of final hunting season in CA (equilibrium population size)
      W2mf0_eq[match(k,ks),match(d,dummy),match(R2_0,R2_0s)] = W2mf0                       # Number of individuals living at end of final hunting season in GC (equilibrium population size)


      # Prepare harvest output
      k_plot = change_params_df["hs_plot", level_] / kill_keep # h_plot / kill_keep; level_ = 2 ; change_params_df["hs_plot",] / kill_keep
      ks_round=round(ks,digits=2) #
      Ks_diff = abs(ks_round-k_plot) # length(ks)
      #k_plot_ind=which(ks_round==k_plot)) 
      k_plot_ind= which(min(Ks_diff)==Ks_diff) # ks[k_plot_ind]*kill_keep; data.frame(ks_round); k_plot; k_plot_ind; 
      #k_plot_ind=9 # kill rate that yields 11% harvest including all cohorts at continental scale; average of estimated annual pintail harvest rates for when bag limit was 1 (see "Runge & Boomer 2005 table appendix C.xlsx")
      
      # Trajectories across years
      #Wmf0s[,match(k,ks),match(Sw2_max,Sw2_maxs)] = Wmf[1:t.max]                 # Number of males and females at end of winter
      #pops_ = Wmf0s
      #colnames(W1mfs)=c("W1mf","W2mf")
   } # End harvest loop ####
  } # End R2_0s loop
 } # End dummy loop ==

 # Prepare output variables ####
  R2_names = c("k", paste("R2",sprintf("%.3f", round(R2_0s,3)),sep="_")) # names(out_lists$Bmf0s_tmax)
  Bmf0s_tmax = cbind(ks, Bmf0s[t.max,,,]*1e-6)
  Rs_tmax = Rs[t.max,,,] # names(Rs_tmax) typeof(Rs_tmax)
  h_avgs_tmax = h_avgs[t.max,,,] # names(h_avgs_tmax); typeof(h_avgs_tmax)
  out_list_ = list(k_plot_ind, R2_names, Bmf0s, Bmf0s_tmax, Rs_tmax, S11afs, h_avgs_tmax) # c(level_char = list(1:4)) h_avgs
    names(out_list_) = c("k_plot_ind","R2_names", "Bmf0s", "Bmf0s_tmax", "Rs_tmax", "S11afs", "h_avgs_tmax")
  out_list_
  
} # End simulation function


######### Run simulation for 3 levels in change_params_df: lcl, mean, ucl ####
out_lists0 = lapply(1:3, simNOPI) # out_lists0 = out_lists
  names(out_lists0) = names(change_params_df) # length(out_lists$lcl) ; names(out_lists0$lcl)
out_lists = transpose(out_lists0) # names(out_lists); out_lists$Rs_tmax; head(out_lists$Bmf0s$ucl); out_lists$Bmf0s_tmax; out_lists$R2_names
  #  names(out_lists)

# Bmf0s_tmax_xlsx_filename = "Bmf0s_tmax.xlsx"

########## Prepare output for export ####


Bmf0s_tmax_list_mats = out_lists$Bmf0s_tmax # dim(Bmf0s_tmax_list_mats$ucl)
Bmf0s_tmax_list_dfs0 = lapply(Bmf0s_tmax_list_mats, function(x) {if(any(class(x)=="matrix")) as.data.frame(x) else x}) # head(Bmf0s_tmax_list_dfs0)
#Bmf0s_tmax_list_dfs = lapply(Bmf0s_tmax_list_dfs0, setNames, c("k", R2_names)) # head(Bmf0s_tmax_list_dfs)
Bmf0s_tmax_list_dfs = mapply(function(x, y) setNames(x, y), Bmf0s_tmax_list_dfs0, out_lists$R2_names) # names(Bmf0s_tmax_list_dfs) str(Bmf0s_tmax_list_dfs)
  # setNames(Bmf0s_tmax_list_dfs0$avg, out_lists$R2_names$avg)
  # t(Bmf0s_tmax_list_dfs)  Bmf0s_tmax_list_dfs$avg  typeof(Bmf0s_tmax_list_dfs) unlist(Bmf0s_tmax_list_dfs) 
  # length(names(Bmf0s_tmax_list_dfs0$ucl)); length(out_lists$R2_names$ucl)
#Bmf0s_tmax_list_dfs = mlply(cbind(Bmf0s_tmax_list_dfs0, out_lists$R2_names), setNames) # names(Bmf0s_tmax_list_dfs0)
Bmf0s_tmax_list_dfs = lapply(Bmf0s_tmax_list_dfs0, function(x) setNames(x, c("k", rownames(R2_scenarios_all_df)))) # names(Bmf0s_tmax_list_dfs) str(Bmf0s_tmax_list_dfs)

Bmf0s_tmax_ucl_scn_only_mat = out_lists$Bmf0s_tmax$ucl[,1:(nrow(R2_scenarios_all_df)+1)]
  
Bmf0s_tmax_diffs_df0 = data.frame((Bmf0s_tmax_ucl_scn_only_mat - out_lists$Bmf0s_tmax$lcl)*1e6) 
  names(Bmf0s_tmax_diffs_df0) = with(out_lists$R2_names, paste(ucl[1:ncol(Bmf0s_tmax_diffs_df0)],lcl,sep="-"))  # head(Bmf0s_tmax_diffs_df0)
    # out_lists$R2_names$lcl[2:ncol(Bmf0s_tmax_diffs_df0)]; out_lists$R2_names$ucl[2:ncol(Bmf0s_tmax_diffs_df0)] 
  Bmf0s_tmax_diffs_df =  cbind( ks,  Bmf0s_tmax_diffs_df0[,2:ncol(Bmf0s_tmax_diffs_df0)]) # format(, digits = 5 # head(Bmf0s_tmax_diffs_df)
  
  # colMeans(cbind(Bmf0s_tmax_list_mats$ucl,Bmf0s_tmax_list_mats$lcl))
Bmf0s_tmax_mean_cls_df =  apply(abind::abind(Bmf0s_tmax_ucl_scn_only_mat,Bmf0s_tmax_list_mats$lcl, along = 3), 1:2, mean)
Bmf0s_tmax_prop_diffs_df = data.frame(abs(Bmf0s_tmax_ucl_scn_only_mat - Bmf0s_tmax_list_mats$lcl) / Bmf0s_tmax_mean_cls_df) #
  names(Bmf0s_tmax_prop_diffs_df) = names(Bmf0s_tmax_diffs_df) # head(Bmf0s_tmax_prop_diffs_df)
  Bmf0s_tmax_prop_diffs_df$ks = Bmf0s_tmax_diffs_df$ks

Bmf0s_tmax_list_all_dfs = append(Bmf0s_tmax_list_dfs, list("diffs" = Bmf0s_tmax_diffs_df, "prop_diffs" = Bmf0s_tmax_prop_diffs_df))

h_avgs_tmax_ucl_df = cbind(h0 = ks * kill_keep, h_est = out_lists$h_avgs_tmax$ucl)
  
########### Export Bmf0s_tmax_list_dfs to Excel with package openxlsx #### 
  workbook <- createWorkbook() # Create a blank workbook
  mapply(function(x, y) {addWorksheet(workbook, x); writeData(workbook, sheet = x, x = y)},
         c(paste(names(change_params_df),"M",sep="_"), "diffs", "prop_diffs"), Bmf0s_tmax_list_all_dfs ) # Add  sheets to the workbook
  
  # Reorder worksheets
  #worksheetOrder(OUT) <- c(2,1)
  
  # Export the file
  saveWorkbook(workbook, "Bmf0s_tmax.xlsx", overwrite = T)
  
###******** Prepare dataframes for plotting ####
  
  Bmf0s_tmax_list_dfs = out_lists$Bmf0s_tmax # names(out_lists) out_lists$R2_names
  
  #Bmf0s_tmax_list_dfs[is.na(Bmf0s_tmax_list_dfs)] = 0  # doesn't work on list of dfs
  Bmf0s_tmax_df = ldply(out_lists$Bmf0s_tmax, data.frame, .id = "Name") #transpose(out_lists$Bmf0s_tmax); ks 
    #  summary(out_lists$Bmf0s_tmax$avg[,2:ncol(out_lists$Bmf0s_tmax$avg)])
  
  # Bmf0s_tmax_avg_all_gt1 = apply(Bmf0s_tmax_list_dfs$avg[,2:ncol(out_lists$Bmf0s_tmax$avg)], 1, function(x) all(x >= 1))
  ks_Bmf0s_tmax_avg_all_scn_gt1 = ks[apply(Bmf0s_tmax_df[Bmf0s_tmax_df$Name == "avg",3:(ncol(out_lists$Bmf0s_tmax$avg)+1)], 1, function(x) all(x >= 1))] # Bmf0s_tmax_avg_all_gt1
  
    #  summary(out_lists$Bmf0s_tmax$avg[all(out_lists$Bmf0s_tmax$avg[,2:ncol(out_lists$Bmf0s_tmax$avg)] >= 1, 1])
  
  Bmf0s_tmax_df$harv_rate= Bmf0s_tmax_df$ks * kill_keep # head( Bmf0s_tmax_df)
  Bmf0s_tmax_df = Bmf0s_tmax_df[,c("harv_rate","Name", colnames(Bmf0s_tmax_df)[3:(ncol(Bmf0s_tmax_df)-1)])]  #cbind(Bmf0s_tmax_df) # head(Bmf0s_tmax_df); as.character(grep("V", colnames(Bmf0s_tmax_df))
    #names(Bmf0s_tmax_df)[grep("R", colnames(Bmf0s_tmax_df))][(ncol(Bmf0s_tmax_diffs_df0)):(ncol(Bmf0s_tmax_df))] = paste("R2",sprintf("%.2f", round(R2_0s_post_hoc,2)),sep="_")
    #names(Bmf0s_tmax_df)[grep("R", colnames(Bmf0s_tmax_df))] = paste("R2",sprintf("%.2f", round(R2_0s_post_hoc,2)),sep="_")
    names(Bmf0s_tmax_df)[3:(2+nrow(R2_scenarios_all_df))] = rownames(R2_scenarios_all_df)
       #names(Bmf0s_tmax_list_dfs$ucl)[2:length(Bmf0s_tmax_list_dfs$ucl)]
  Bmf0s_tmax_long_df = melt(Bmf0s_tmax_df, id=c("harv_rate","Name")) # head(Bmf0s_tmax_long_df); tail(Bmf0s_tmax_long_df)
  Bmf0s_tmax_long_df[is.na(Bmf0s_tmax_long_df)] = 0
    names(Bmf0s_tmax_long_df)[2:4] = c("level", "scenario", "Bmf0_tmax") # [names(Bmf0s_tmax_long_df) %in% c("variable","value")]
    apriori_scenario_names = c("No Conservation", "Obs. Conditions", "Inc. Conservation", "No Winter Wheat", "Inc. Winter Wheat", "Inc. Winter Wheat - Targeted")
    levels(Bmf0s_tmax_long_df$scenario)[1:nrow(scenarios_concise_df)] <- apriori_scenario_names  #  reversing order due to facet_wrap function in ggplot
    #levels(Bmf0s_tmax_long_df$scenario)[grepl("pct_crop_to_grass",rownames(R2_scenarios_all_df))] <- rownames(R2_crazy_df3)  #  reversing order due to facet_wrap function in ggplot

  Rs_tmax_df =  ldply(out_lists$Rs_tmax, data.frame, .id = "Name") # head(Rs_tmax_df)
   #Rs_tmax_df = Rs_tmax_df0[,c("Name", colnames(Rs_tmax_df0)[2:(ncol(Rs_tmax_df0)-1)])] # head(Rs_tmax_df)
   names(Rs_tmax_df)[2:(1+nrow(R2_scenarios_all_df))] = rownames(R2_scenarios_all_df)
  Rs_tmax_long_df = melt(Rs_tmax_df, id="Name") # head(Rs_tmax_long_df); tail(Rs_tmax_long_df)
    names(Rs_tmax_long_df) = c("level", "scenario", "R_tmax") # [names(Bmf0s_tmax_long_df) %in% c("variable","value")]
  Bmf0s_tmax_long_df$R_tmax = Rs_tmax_long_df$R_tmax
  

  Bmf0s_tmax_long_apriori_df = Bmf0s_tmax_long_df[Bmf0s_tmax_long_df$scenario %in% apriori_scenario_names,] # head(Bmf0s_tmax_long_apriori_df) tail(Bmf0s_tmax_long_apriori_df) str(Bmf0s_tmax_long_apriori_df)
    Bmf0s_tmax_long_apriori_df$scenario = factor(Bmf0s_tmax_long_apriori_df$scenario) # levels(Bmf0s_tmax_long_apriori_df$scenario)
    Bmf0s_tmax_long_apriori_df$bag_limit =  NA
    Bmf0s_tmax_long_apriori_df$bag_limit[round(Bmf0s_tmax_long_apriori_df$harv_rate,3)  %in%  hs_plot] =  1
    Bmf0s_tmax_long_apriori_df$bag_limit[Bmf0s_tmax_long_apriori_df$harv_rate==0] = 0
    
  Bmf0s_tmax_wide_apriori_df = dcast(Bmf0s_tmax_long_apriori_df, harv_rate + bag_limit + scenario ~ level, value.var = "Bmf0_tmax") # https://seananderson.ca/2013/10/19/reshape/; head(Bmf0s_tmax_wide_apriori_df) , paste0("Bmf0_tmax_", level)
    # str(Bmf0s_tmax_wide_apriori_df) head(Bmf0s_tmax_wide_apriori_df)
    Bmf0s_tmax_wide_apriori_df$harv_rate_char = paste("harv_rate", as.character(round(Bmf0s_tmax_wide_apriori_df$harv_rate,3)), sep="_")
      write.xlsx(Bmf0s_tmax_wide_apriori_df, "Bmf0s_tmax_wide_apriori_df.xlsx")
    Bmf0s_tmax_wide_apriori_baglimit_df = subset(Bmf0s_tmax_wide_apriori_df, !is.na(bag_limit))

  Bmf0s_tmax_long_posthoc_df = Bmf0s_tmax_long_df[grepl("pct_crop_to_grass", Bmf0s_tmax_long_df$scenario),] # head(Bmf0s_tmax_long_posthoc_df) tail(Bmf0s_tmax_long_posthoc_df) str(Bmf0s_tmax_long_posthoc_df)
    Bmf0s_tmax_long_posthoc_df$scenario = factor(Bmf0s_tmax_long_posthoc_df$scenario) # levels(Bmf0s_tmax_long_posthoc_df$scenario)
    Bmf0s_tmax_long_posthoc_df$bag_limit =  NA
    Bmf0s_tmax_long_posthoc_df$bag_limit[round(Bmf0s_tmax_long_posthoc_df$harv_rate,3)  %in%  hs_plot] =  1 # unique(Bmf0s_tmax_long_posthoc_df$harv_rate)
    Bmf0s_tmax_long_posthoc_df$bag_limit[Bmf0s_tmax_long_posthoc_df$harv_rate==0] = 0
    Bmf0s_tmax_long_posthoc_df$pct_crop_to_grass = as.numeric(sub("_.*", "", Bmf0s_tmax_long_posthoc_df$scenario)) # str(Bmf0s_tmax_long_posthoc_df)
     # Bmf0s_tmax_long_posthoc_df[with(Bmf0s_tmax_long_posthoc_df, order(bag_limit, level)), ]
    R2_crazy_mat = as.matrix(R2_crazy_df3)
    R2_crazy_long_df = melt(R2_crazy_mat) # head(R2_crazy_long_df) note that melt creates a data frame from the matrix
      names(R2_crazy_long_df) = c("scenario", "level", "R2")
    Bmf0s_tmax_long_posthoc_df = merge(Bmf0s_tmax_long_posthoc_df, R2_crazy_long_df, by = c("scenario", "level"))
    
    
  Bmf0s_tmax_long_posthoc_bag_limit_df = subset(Bmf0s_tmax_long_posthoc_df, !is.na(bag_limit)) # head(Bmf0s_tmax_long_posthoc_bag_limit_df)
    #Bmf0s_tmax_long_posthoc_bag_limit_df$harv_rate_char = paste("harv_rate", as.character(round(Bmf0s_tmax_long_posthoc_bag_limit_df$harv_rate,3)), sep="_")
    #Bmf0s_tmax_long_posthoc_bag_limit_df$harv_rate_char = paste("h", as.character(sprintf("%.2f", round(Bmf0s_tmax_long_posthoc_bag_limit_df$harv_rate,2))), sep=" = ") # italic('h')
    harv_rate_empirical_char = paste("h", sprintf("%.2f", hs_empirical_plot), sep=" = ") # italic('h')
    Bmf0s_tmax_long_posthoc_bag_limit_df = Bmf0s_tmax_long_posthoc_bag_limit_df[with(Bmf0s_tmax_long_posthoc_bag_limit_df, order(bag_limit, level, scenario)), ]
    Bmf0s_tmax_long_posthoc_bag_limit_df$USD_B =  1e-9 * cost_per_ac_SScrop_idle_grass * tot_SScrop_ac_PPR*  Bmf0s_tmax_long_posthoc_bag_limit_df$pct_crop_to_grass/100  # head
     # subset(Bmf0s_tmax_long_posthoc_bag_limit_df, Bmf0_tmax>=4 & bag_limit==1)
     # subset(Bmf0s_tmax_long_posthoc_bag_limit_df, pct_crop_to_grass == 100 & harv_rate == 0.05)$Bmf0_tmax
    
  write.xlsx(Bmf0s_tmax_long_posthoc_bag_limit_df, "Bmf0s_tmax_long_posthoc_bag_limit_df.xlsx")
  
  #Bmf0s_tmax_long_posthoc_noHarv_df = subset(Bmf0s_tmax_long_posthoc_bag_limit_df, bag_limit==0)
    # Bmf0s_tmax_long_posthoc_bag_limit_df[with(Bmf0s_tmax_long_posthoc_bag_limit_df, order(bag_limit, level, scenario)), ]
    # Bmf0s_tmax_long_posthoc_df[rownames(Bmf0s_tmax_long_posthoc_df)=="967",]
    # Bmf0s_tmax_long_df[rownames(Bmf0s_tmax_long_df) %in% c("415", "967"),]
  
        
  #Bmf0s_tmax_noHarv_long_df = melt(Bmf0s_tmax_df[Bmf0s_tmax_df$ks==0,])
  #names(Bmf0s_tmax_noHarv_long_df) = c("level", "R2", "Bmf0s_tmax") # head(Bmf0s_tmax_noHarv_long_df)

############# Set up graph windows and parameters ####
  # Graphing with ggplot: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization
  # options(device = "windows")
  #windows(width=10,height=7.5)                                         # PPT standard is 10 x 7.5; fin, pin, din
  outfile="NOPI_population_model_graphs.pdf"
  while (.Device=="pdf"){dev.off()}     
  pdf(file=outfile,width=10,height=7.5)
  # mar=c(5,6,4,10)
  par(mar=c(5,4,4,2) + 0.5)                                           #c(bottom, left, top, right) default: par(mar=c(5,4,4,2) + 0.1) 
  lty_ = c("solid","solid","dashed","dashed")
  lwd_ = c(4,2,4,2)
  font_size = 10
  #R2_0s=round(R2_0s,2)
############## Plot harvest rate vs BPOP by habitat scenario Fig 2 ####
  p_ <- ggplot(Bmf0s_tmax_long_apriori_df, aes(x=harv_rate*100, y=Bmf0_tmax, group=level)) + # head(Bmf0s_tmax_long_apriori_df) !is.na(Bmf0s_tmax_long_apriori_df) unique(Bmf0s_tmax_long_apriori_df$scenario)
    labs(x="Percent adult female pintails harvested", y="Continental BPOP (Millions)") +
    ylim(0,max(Bmf0s_tmax_long_apriori_df$Bmf0_tmax)) +
    #xlim(0,10) +
    scale_x_continuous(breaks = seq(0,10, by=2), limits = c(0,10)) + 
    geom_line(aes(linetype=level), size=1, show.legend = FALSE) +  # , size=1
    scale_linetype_manual(values=c("dashed", "solid", "dashed")) + # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
    facet_wrap( ~ scenario, ncol=2) + # fct_rev( , scales="free", as.table =F
    theme_bw() +
    theme(strip.background = element_blank(),
          text = element_text(size = font_size),
          panel.border = element_rect(colour = "black"), strip.text.x=element_text(size=font_size,face="bold"),
          axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=12,face="bold"))  # panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
  while (.Device=="windows"){dev.off()}; windows(width=14, height = 14); p_ # close all external graph windows and create new plot; rescale can be one of these: "R" [default], "fit", "fixed"; width & height are in inches

################ Plot % grassland conversion (and cost) vs continental BPOP including CLs; Fig 3 ####
  #R2_plot_long_df = 
    # str(Bmf0s_tmax_long_posthoc_bag_limit_df)
 
  # plot , color="black"
  p_ <- ggplot(Bmf0s_tmax_long_posthoc_bag_limit_df, aes(x=pct_crop_to_grass, y=Bmf0_tmax, group=level)) + # head(Bmf0s_tmax_long_posthoc_df) !is.na(Bmf0s_tmax_long_posthoc_df) unique(Bmf0s_tmax_long_apriori_df$scenario)
      # subset(Bmf0s_tmax_long_posthoc_bag_limit_df, scenario == "100_pct_crop_to_grass" & harv_rate_char == "h = 0.03"); tail(Bmf0s_tmax_long_posthoc_bag_limit_df)
    labs(x="Percent spring-seeded cropland converted to idle grassland", y="Continental BPOP (Millions)") +
    #ylim(0,max(Bmf0s_tmax_long_posthoc_bag_limit_df$Bmf0_tmax)) +
    scale_y_continuous(breaks = seq(0, round(max(Bmf0s_tmax_long_posthoc_bag_limit_df$Bmf0_tmax)), by = 2)) +
    scale_x_continuous(breaks = seq(0,100, by=20 ), sec.axis = sec_axis(~ . * 1e-9 * cost_per_ac_SScrop_idle_grass * tot_SScrop_ac_PPR / 100, name = "Cost (Billions of dollars)")) + # p_$layers
    geom_line(aes(linetype=level), size=1, show.legend = FALSE) +  # , size=1
    scale_linetype_manual(values=c("dashed", "solid", "dashed")) + # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
    facet_wrap( ~ harv_rate_char, ncol=2) + # fct_rev( , as.table =F , ncol=2, scales="free"
    theme_bw() +
    #geom_text(aes(label = harv_rate_char), x = median(pct_crop_to_grass), y = Inf, hjust = 0.5, vjust = 1.5, fontface = "italic") + # , colour="black" geom_text_repel(parse = TRUE)
    #geom_text_repel(parse = TRUE, aes(label = harv_rate_char), x = max(pct_crop_to_grass)-10, y = Inf, hjust = 0.5, vjust = 1.5) + # , colour="black" ()
    
    theme(strip.background = element_blank(),
          text = element_text(size = font_size),
          #plot.title = element_text(margin = margin(b = -100)), # hjust & vjust have been replaced by margin function
          panel.border = element_rect(colour = "black"), 
          #strip.text.x=element_text(size=font_size,face="bold", margin = margin(t = 5, b = -2)),
          strip.text = element_blank(), 
          axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=12,face="bold"))  # panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
  while (.Device=="windows"){dev.off()}; windows(width=14, height = 14); p_ # close all external graph windows and create new plot; rescale can be one of these: "R" [default], "fit", "fixed"; width & height are in inches

  
####### Plot BPOP by habitat & harvest scenario Fig X ####
  p_ <- ggplot(Bmf0s_tmax_wide_apriori_baglimit_df, aes(x=scenario, y=avg, group=harv_rate_char)) + # head(Bmf0s_tmax_long_apriori_df) !is.na(Bmf0s_tmax_long_apriori_df) unique(Bmf0s_tmax_long_apriori_df$scenario) , group=level
    labs(x="Harvest rate", y="Continental BPOP (Millions)") +
    #ylim(0,max(Bmf0s_tmax_long_apriori_df$Bmf0_tmax)) +
    geom_pointrange(aes(ymin=lcl, ymax=ucl)) + # , shape=harv_rate_char
    facet_wrap( ~ harv_rate_char, ncol=2, scales="free") + # fct_rev( , as.table =F
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #scale_shape_identity()
  while (.Device=="windows"){dev.off()}; windows(width=14, height = 14); p_ # close all external graph windows and create new plot; rescale can be one of these: "R" [default], "fit", "fixed"; width & height are in inches
  
  
  
  
#### Plot age ratio vs BPOP by harvest scenario Fig Y ####
  p_ <- ggplot(Bmf0s_tmax_long_posthoc_bag_limit_df, aes(x=R2, y=Bmf0_tmax, group=level)) + # head(Bmf0s_tmax_long_posthoc_bag_limit_df) !is.na(Bmf0s_tmax_long_posthoc_df) unique(Bmf0s_tmax_long_apriori_df$scenario)
    labs(x="Fall age ratio", y="Continental BPOP (Millions)") +
    #ylim(0,max(Bmf0s_tmax_long_posthoc_bag_limit_df$Bmf0_tmax)) +
    scale_y_continuous(breaks = seq(0, round(max(Bmf0s_tmax_long_posthoc_bag_limit_df$Bmf0_tmax)), by = 2)) +
    geom_line(aes(linetype=level), size=1, show.legend = FALSE) +  # , size=1
    scale_linetype_manual(values=c("dashed", "solid", "dashed")) + # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
    facet_wrap( ~ harv_rate_char, ncol=2) + # fct_rev( , as.table =F , ncol=2, scales="free"
    theme_bw() +
    #geom_text(aes(label = harv_rate_char), x = median(R2), y = max(Bmf0_tmax), hjust = 0.5, vjust = 1.5, fontface = "italic") + # , colour="black" geom_text_repel(parse = TRUE) median(Bmf0s_tmax_long_posthoc_bag_limit_df$R2)
    #geom_text_repel(parse = TRUE, aes(label = harv_rate_char), x = max(pct_crop_to_grass)-10, y = Inf, hjust = 0.5, vjust = 1.5) + # , colour="black" ()
    
    theme(strip.background = element_blank(),
          text = element_text(size = font_size),
          #plot.title = element_text(margin = margin(b = -100)), # hjust & vjust have been replaced by margin function
          panel.border = element_rect(colour = "black"), 
          strip.text.x=element_text(size=font_size,face="bold", margin = margin(t = 5, b = -2)),
          strip.text = element_blank(), 
          axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=12,face="bold"))  # panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
  while (.Device=="windows"){dev.off()}; windows(width=14, height = 14); p_ # close all external graph windows and create new plot; rescale can be one of these: "R" [default], "fit", "fixed"; width & height are in inches
  
  
  
      ## Sensitivity Bmf0s_tmax_df ####
  
  p_ <- ggplot(subset(Bmf0s_tmax_noHarv_long_df, !is.na(Bmf0s_tmax)), aes(x=level, y=Bmf0s_tmax, group=R2)) +
    geom_line(aes(linetype=R2)) #+
    #geom_point(aes(shape=supp))
  windows(); p_
  

#** Realized continental density dependence ####
  # out_lists$Rs_tmax Bmf0s_tmax_list_dfs
  p_ <- ggplot(Bmf0s_tmax_long_df, aes(x=R_tmax, y=Bmf0_tmax, group=level)) + # head(Bmf0s_tmax_long_df) !is.na(Bmf0s_tmax_long_df) unique(Bmf0s_tmax_long_df$scenario)
    labs(x="Continental age ratio", y="Continental BPOP (Millions)") +
    #ylim(0,max(Bmf0s_tmax_long_apriori_df$Bmf0_tmax)) +
    #xlim(0,10) +
    geom_line(aes(linetype=level), size=1, show.legend = FALSE) +  # , size=1
    scale_linetype_manual(values=c("dashed", "solid", "dashed")) + # "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
    facet_wrap( ~ scenario) + # fct_rev( , scales="free", as.table =F , ncol=2
    theme_bw() +
    theme(strip.background = element_blank(),
          text = element_text(size = font_size),
          panel.border = element_rect(colour = "black"), strip.text.x=element_text(size=font_size,face="bold"),
          axis.text=element_text(size=14,face="bold"), axis.title=element_text(size=12,face="bold"))  # panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
  while (.Device=="windows"){dev.off()}; windows(width=14, height = 14); p_ # close all external graph windows and create new plot; rescale can be one of these: "R" [default], "fit", "fixed"; width & height are in inches
  
  
  
  

# Population trajectories ####
  # No harvest
    # Continental at start of breeding
    #par(mar=par("mar")+c(1,1,1,8))           #c(bottom, left, top, right) default: par(mar=c(5,4,4,2) + 0.1)  

    y_max=max(Bmf0s_[2:t.max,1,,])

    Bmf0s_no_harv=Bmf0s_[2:t.max,1,,]
    Bmf0s_mod_harv=Bmf0s_[2:t.max,k_plot_ind,,]
    
    # Temp ####
    R2s_mod_harv=R2s[2:t.max,k_plot_ind,,]  # dim(R2s_mod_harv)
    R2s_no_harv=R2s[2:t.max,1,,]  # dim(R2s_mod_harv)
    

    l=0 # graphics.off(); par("mar"); par(mar=c(1,1,1,1)+1.5)
    plot(1,type="n",main="Continental breeding trajectories (no harvest)", xlab="Year",ylab="Start of breeding N",font=2,font.lab=2,cex.lab=2,cex.axis=2,
        xlim=c(min(years[2:t.max]),max(years[2:t.max])),ylim=c(min(Bmf0s_mod_harv,na.rm = TRUE),max(Bmf0s_no_harv,na.rm = TRUE)))
    #for (i in 1:length(dummy)){
      for (j in 1:length(R2_0s)){
         l=l+1
        lines(years[2:t.max],Bmf0s_no_harv[,j],type="o",col="black",pch=NA) #,)                                         # Equilibrium population size vs harvest rate dim(Bmf0s_no_harv)
            # ,lwd=lwd_[l],lty=lty_[l]
        text(t.max-1,Bmf0s_no_harv[t.max-1,j], round(R2_0s[j],2), pos=4, cex=0.8) # round(R2_0s,2)
      }
    #} 
    #legend(x=t.max*1.05, y=y_max, R2_0s, title="PR R",xpd=T)     # lwd=lwd_-1,lty=lty_,   

  # Medium harvest
    # Continental at Start of breeding
    heading <- paste("Continental breeding trajectories (",round(100*ks[k_plot_ind]*kill_keep,digits=2), "pct harvest)") #
    l=0
    plot(1,type="n",main=heading, xlab="Year",ylab="Start of breeding N",font=2,font.lab=2,cex.lab=2,cex.axis=2,
         xlim=c(min(years[2:t.max]),max(years[2:t.max])),ylim=c(min(Bmf0s_mod_harv,na.rm = TRUE),max(Bmf0s_no_harv,na.rm = TRUE)))
    #for (i in 1:length(dummy)){
    for (j in 1:length(R2_0s)){
      l=l+1
      lines(years[2:t.max],Bmf0s_mod_harv[,j],type="o",col="black",pch=NA) #,)                                         # Equilibrium population size vs harvest rate dim(Bmf0s_no_harv)
      # ,lwd=lwd_[l],lty=lty_[l]
      text(t.max-1,Bmf0s_mod_harv[t.max-1,j], round(R2_0s[j],2), pos=4, cex=0.8) # round(R2_0s,2)
    }
    #} data.frame(Bmf0s_mod_harv[t.max-1,], R2_0s)
    #legend(x=t.max*1.05, y=y_max, Sw2_0s_R2_0s, lwd=lwd_-1,lty=lty_, title="GC S int; PR R int",xpd=T)       
    #savePlot(filename=heading,type="pdf")
    #win.graph()    


  # PPR age ratio vs Breeding N*  ####
    l=0 # t.max=99
    #plot(R2s_mod_harv[t.max-1,],Bmf0s_mod_harv[t.max-1,])
    plot(1,type="n",main="Breeding N* vs. PPR age ratio", xlab="PPR age ratio",ylab="Breeding N*",font=2,font.lab=2,cex.lab=2,cex.axis=2,
         xlim=c(0,max(R2_0s,na.rm = TRUE)),ylim=c(min(Bmf0s_mod_harv,na.rm = TRUE),max(Bmf0s_mod_harv,na.rm = TRUE)))
        lines(R2s_mod_harv[1,],Bmf0s_mod_harv[t.max-1,],type="o",col="black",pch=NA) #,lwd=lwd_[l],lty=lty_[l]) #,)                                         # Equilibrium population size vs harvest rate
        # data.frame("PPR_age_ratio"=R2s_mod_harv[1,], "Continental_BPOP_yr100" = Bmf0s_mod_harv[t.max-1,])  

      #plot(1,type="n",main="Breeding N* vs. PPR age ratio with no harv", xlab="PPR age ratio",ylab="Breeding N*",font=2,font.lab=2,cex.lab=2,cex.axis=2,
        #   xlim=c(0,max(R2_0s,na.rm = TRUE)),ylim=c(min(Bmf0s_no_harv,na.rm = TRUE),max(Bmf0s_no_harv,na.rm = TRUE)))
      lines(R2s_no_harv[1,],Bmf0s_no_harv[t.max-1,],type="o",col="black",pch=NA,lty=2) #,lwd=lwd_[l],lty=lty_[l]) #,)                                         # Equilibrium population size vs harvest rate
      # data.frame("PPR_age_ratio"=R2s_no_harv[1,], "Continental_BPOP_yr100" = Bmf0s_no_harv[t.max-1,])  
        
      
#savePlot(filename="Breeding N vs Harvest rate",type="pdf")
##win.graph()
while (.Device=="pdf"){dev.off()}     
#dev.off()
cmd=paste("open",outfile)
system(cmd) 

# Print annual survival rates

  #S11afs_R2_0_max=as.matrix(S11afs[,,,match(R2_0_max,R2_0s)])
  #stat.desc(S11afs_R2_0_max)
  
  S11afs2=as.matrix(S11afs[,,,]) # head(S11afs)
  stat.desc(S11afs)
  
  S11afs_baseline=as.matrix(S11afs[t.max,,match(d,dummy),match(scenarios_df$recruit_rate_avg[2],R2_0s)])  # head(S11afs_baseline)
  stat.desc(S11afs_baseline)

#data.frame(ks,h_prop_eq[,1,length(R2_0s)]) # dim(h_prop_eq)


  # Beginning of breeding season
    stat.desc(Bm0_Bf0_s[t.max,,,])
      
save.session("pintail linkages.RData")   # stores all variables & loaded packages in this R file ####       