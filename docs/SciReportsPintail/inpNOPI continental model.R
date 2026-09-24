  # Primary user-assigned parameter values

  # Breeding
     # Initial breeding population size based on USFWS 2009 waterfowl status report
    #B0=3.225e6
     # Long-term BPOP 1955-2014 for males + females from NAWMP 2014 revised objectives report
    B00 = 4.003e6   # goal for continental size of pintail breeding population
    B0 = B00
    p_B1_vs_B2andB3   = 0.4052288                                                           # 2009 estimate from USFWS report  9.3e5/2.295e6
    p_B2_vs_B3        = 45/60 # simulation prediction at MSY #0.6 -- simulation prediction at K # 0.86 -- 2009 estimate from M Runge = BPOP / BPOPcorr   = 0.86

    # Survival probabilities
    Sbm = 0.98
    Sbf = 0.81

    # Reproduction
      #AK
        R1_0    =  0                # -0.03 -0.05
        R1_B1   = -0.15*1e-6                 # -0.1

      #PPR
        R2_0    =  0           # 0.06 round(R2_0_min+log(1.1),2)
        R2_B2   = -0.12 #*1e-6                 # -0.24
        R2_P    =  0.01
        P=3.2                                                                          # Number of Canadian ponds from Runge & Boomer 2005
      #NU
        R3_0    = -1
        R3_B3   = -0.08*1e-6               #-0.08   -.15

  # Fall migration
    kill_keep = 0.8                                                                   # Complement of 20% crippling loss
    m11 = 0.9                         # From AK to CA
    m22 = 0.5                         # From PR to GC      0.5  0.85

  # Fall Survival probabilities
    x = 0.04
    # Adults
        Sbw11a = 0.9 + x # From AK+NU to CA
        Sbw22a = 0.9 + x  # From PR to GC
        Sbw12a = 0.85 + x  # From AK+NU to GC
        Sbw21a = 0.9 + x  # From PR to CA
        Sbw32a = 0.9 + x  # From UN to GC
        Sbw31a = 0.9 + x  # From UN to CA

    # Juveniles
        Sbw11j = 0.8 + x  # From AK+NU to CA
        Sbw22j = 0.8 + x  # From PR to GC
        Sbw12j = 0.75 + x  # From AK+NU to GC
        Sbw21j = 0.8 + x  # From PR to CA
        Sbw32j = 0.8 + x  # From NU to GC
        Sbw31j = 0.8 + x  # From NU to CA

  # Harvest vulnerabilities vs. adult females  based on Jon Runge's unpublished estimates
      Vaf = 1
      Vam = 1.25            #1.25
      Vjm = 2.75             #2.75
      Vjf = 2               #2


  # Winter-spring survival
    Sw_min  =  0.84
    Sw_max  =  0.96
    SwWmf   = -1.5*1e-6                     # -1.5 Slope of density dependence for winter-spring survival -- seems to have strong effect on carrying capacity                                                                  # Intercept of density dependence for winter-spring survival
    #CA
      Sw1_0   =  4
      #Sw1W1mf = -1.5*1e-6
    #GC SW2_0 3 or 5
      Sw2_0   =  3               # Sw2_0_min*1.1
      #Sw2W2mf = -1.5*1e-6

  # Spring migration
    n11 = 0.54                              # 0.74                                         # n11 = 0.74 Derived from Miller et al 2005 Fig 7
    n22 = 0.9

    # Density dependence PR to NU
      psi_PRdprt_max = 0.7                    # .7                                        # Maximum probability
      psi_PRdprt_0 = -3                        # -3                                         # Intercept
      psi_PRdprt_N_arrive2 = 1e-6             # 1e-6                                          # Slope for effect of PR arrival density
      psi_PRdprt_P = .1                                                                # Slope for effect of ponds
      psi_PRtoNU = 0.9                                                                  # Proportion of overflight birds that go to NU vs to AK
