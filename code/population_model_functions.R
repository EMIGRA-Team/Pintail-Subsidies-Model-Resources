# Functions originally developed by Joanna Bieri and Christine Sample then modified by B. Mattsson who merged Cr_pintail.r and CR_ONEEQ.r into Cr_fn
# Specify demographic functions for pintails ####
#source("SpeciesFunctions.R")

## CALCULATE f -> NODE DYNAMICS UPDATE #################

f_function <- function() {
  # TYPICAL VARIABLES USED (N,alpha,type,ind,t)
  # N - contains the population arriving at the node at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE if you specify "class_name" the it will look to that specific class's population each time
  # NOTE if you set "class_name"=[[type]] it will get the numbers for the current calculation's class
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # NOTE if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants -> BM writes: wrong comment here
  # ind - gives the season number as an integer
  # t - gives the time step
  
  # Initiaize basic update information ####
  Repro <- 0*N[[t]][[type]]
  Survi <- alpha[[type]][[ind]]$S*N[[t]][[type]]
  Trans <- 0*N[[t]][[type]]
  
  # print("Pre")
  # print(paste("t=",t,"type=",type))
  # print(Repro)
  # print(Survi)
  # print(Trans)
  
  
  # Breeding Nodes ####
  if(ind==1){
    if(type==3 || type==4){ # If we are updating the Juvenile population
      
      POP <- N[[t]][[1]]+N[[t]][[2]] # Number of existing Males and Females for density dependence
      Survi_F <- alpha[[1]][[ind]]$S # Female survival Rate
      
      R <- alpha[[1]][[ind]]$R # only adult females can produce young during breeding
      
      Repro <- R*Survi_F*N[[t]][[1]]
      
    } 
  }
  
  # Winter Nodes ####
  # Density dependent node survival rates only apply to the post harvest winter-spring season
  if(ind==2){
    
    beta0 <- alpha[[type]][[ind]]$b_0
    beta1 <- alpha[[type]][[ind]]$b_1
    smax <- alpha[[type]][[ind]]$S_max
    smin <- alpha[[type]][[ind]]$S_min
    POP <- N[[t]][[1]]+N[[t]][[2]]+N[[t]][[3]]+N[[t]][[4]]
    
    Z <- beta0+beta1*POP 
    Survi <- (smin+((smax)-(smin))/(1+exp(-Z)))*N[[t]][[type]]
    
    
    # Transition Rates Juvenile -> Adult in Winter/Spring
    if(type==3 || type==4){ # All Juveniles must transition.
      Survi <- 0*N[[t]][[type]]
      Repro <- 0*N[[t]][[type]]
      Trans <- 0*N[[t]][[type]]
    }
    if(type==1){
      Survi_J <- (smin+((smax)-(smin))/(1+exp(-Z)))
      Trans <- Survi_J*N[[t]][[3]] # Transition the Juveniles to Adult Females
    }
    if(type==2){
      Survi_J <- (smin+((smax)-(smin))/(1+exp(-Z)))
      Trans <- Survi_J*N[[t]][[4]] # Transition the Juveniles to Adult Males
    }
  }
  
  f_new <- Survi + Repro + Trans # demog_list <- tibble::lst(Survi, Repro, Trans) 
  #print(f_new)
  # Units of f_new should be total number of migrants leaving the node
  
  # return ####
  return(f_new)
}

## CALCULATE p -> PATH TRANSITION RATES #################
p_function <- function(){
  # TYPICAL VARIABLES USED (N,f_update,alpha,beta,type,ind,t)
  # N - contains the population arriving at the nodes at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE this population count is before accounting for node dynamics
  # f_update - contains the population after node dyanamics have been applied for the current time step - f_update[["class_number"]]
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # beta - contains all information about path dynamics - beta[["class_number"]][["season]]$"variable_name"
  # NOTE edge transition rates are found in beta[["class_number"]][["season]]$p_ij
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # for example: if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants
  # ind - gives the season number as an integer
  # t - gives the time step
  # NOTE if it is required to change any variable globally then use "old_value" <<- "new_value"
  
  p <- beta[[type]][[ind]]$p_ij  #p_edge[[type]][[ind]]
  
  # density dependent transitions only apply to the spring stopover season
  if(ind==3){
    delta0 <- alpha[[type]][[ind]]$Delta_0[2]
    delta1 <- alpha[[type]][[ind]]$Delta_1[2]
    delta2 <- alpha[[type]][[ind]]$Delta_2[2]
    
    # Number of migrants who survived PR
    PRPOP <- f_update[[1]][2]+f_update[[2]][2]
    ponds <- alpha[[type]][[ind]]$P[2]
    
    Y <- delta0 + delta1*PRPOP+delta2*ponds
    
    # Original transition probabilities from PR
    psim <- alpha[[type]][[ind]]$psi_max[2]
    newpsi_leave <- (psim)*1/(1+exp(-Y))
    
    # Probabilities of leaving PR for AK or NU
    psi21 <- beta[[type]][[ind]]$psi_ij[2,1]  
    psi23 <- beta[[type]][[ind]]$psi_ij[2,3]
    
    # Update the transition probabilities
    p[2,1] <- newpsi_leave*psi21
    p[2,3] <- newpsi_leave*psi23
    p[2,2] <- 1-newpsi_leave
    
  }
  
  p_new <- p
  # p_new should be unitless and represent the edge transition probabilities for the given step
  
  return(p_new)
}

## CALCULATE s -> PATH SURVIVAL RATES #################
s_function <- function(){
  # TYPICAL VARIABLES USED (N,f_update,alpha,beta,type,ind,t)
  # N - contains the population arriving at the nodes at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE this population count is before accounting for node dynamics
  # f_update - contains the population after node dyanamics have been applied for the current time step - f_update[["class_number"]]
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # beta - contains all information about path dynamics - beta[["class_number"]][["season]]$"variable_name
  # NOTE edge survival rates are found in beta[["class_number"]][["season]]$s_ij
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # NOTE if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants
  # ind - gives the season number as an integer
  # t - gives the time step
  # NOTE if it is required to change any variable globally then use "old_value" <<- "new_value"
  
  # Edge survival where k=hunting mortality
  s <- beta[[type]][[ind]]$s_ij*(1-beta[[type]][[ind]]$kappa_ij)  #s_edge[[type]][[ind]]
  
  s_new <- s
  # s_new should be unitless and represnt the edge survival for the given step
  return(s_new)
}

# specify Cr functions ####
## Shifter function  #####
# Shift vector x by n units to the left
shifter <- function(x, n = 0) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

## Coid Function   ####
# INPUTS - the pathway (in vector form), the beginning season, the breeding season, sA_node, sJ_node, R0, sA_edge, sJ_edge
Coid <- function(path, theSeason, breedSeason, sA_node, sJ_node, R0, sA_edge, sJ_edge)  
{
  a <- 1:(length(path)-1)
  seasonvec <- shifter(a, theSeason-1) #theSeason is the anniversary date
  Aoid <- 1  #initialize contribution
  Joid <- 1  
  breedOccurred <- 0  #breeding season has not yet occurred
  for (i in a){ 
    Aoid_mat <- diag(sA_node[,seasonvec[i]]) %*% as.matrix(sA_edge[,,seasonvec[i]])
    if(seasonvec[i] == breedSeason){  #Check if the breeding season occurred and define Juvenile matrix accordingly
      breedOccurred <- 1
      Joid_mat <- diag(sA_node[,seasonvec[i]]*R0[,seasonvec[i]]) %*% as.matrix(sJ_edge[,,seasonvec[i]])
    }
    else if(breedOccurred == 1){
      Joid_mat <- diag(sJ_node[,seasonvec[i]]) %*% as.matrix(sJ_edge[,,seasonvec[i]])
    }
    else {
      Joid_mat <- Aoid_mat
    }
    Aoid <- Aoid*Aoid_mat[path[i],path[i+1]]
    Joid <- Joid*Joid_mat[path[i],path[i+1]]
  }
  return(Aoid + Joid)  # OUTPUT - "path" pathway contribution 
}

## Cr function  #### 
Cr_fn <- function(seasons, nSites, num_nodes, timestep, NUMNET, sex, N){
  # Front matter ####
  # This code calculates the Cr for each node at each season
  # using the ONE EQUATION / MULTI-ANNIVERSARY APPROACH
  # Originally by Joana Bieri & Christine Sample, modified by B. Mattsson as follows:
  # 1) Convert Cr calculations to a function that can be called by PintailSimulation.r
  # 2) streamlined headings for easier navigation
  #
  #Parameters that this code requires to be defined elsewhere:
  #
  # seasons - number of time periods in a cycle
  # nSites - number of nodes
  # breedSeason - the season number during which breeding occurs
  # p - matrix of transition probabilities for each season
  # sA_edge - matrix of adult edge survival for each season
  # sJ_edge - matrix of juvenile edge survival for each season
  # sA_node - vector of adult node survival for each season
  # sJ_node - vector of juvenile node survival for each season
  # R0 - vector of reproduction (birth rate) at each node for each season
  
  # Initialize ####
  CR <- matrix(0,nSites,seasons)
  COI <- array(0,c(nSites,nSites,seasons))
  a <- 1:seasons
  ones <- matrix(1,nSites,1)  #unit column vector
  
  NT <- list()
  LAMBDA <- seq(0,0,length.out=seasons)
  breedSeason <- 1
  sA_node <- matrix(0,nSites,seasons)
  sJ_node <- matrix(0,nSites,seasons)
  sA_edge <- array(0,c(nSites,nSites,seasons))
  sJ_edge <- array(0,c(nSites,nSites,seasons))
  p <- array(0,c(nSites,nSites,seasons))
  R0 <- matrix(0,nSites,seasons)
  
  # Calculate Cr ####
  ## Calculate the CR for each season as anniversary date ####
  for (k in 1:seasons){ 
    
    # Find which simulation time step gives the correct start time
    # Here we augment t to allow for a full year of seasons
    starttime <- timestep - seasons + k -1
    t <- starttime
    if((t+seasons-1)>(timestep-1)){t=t-seasons}
    
    for (i in shifter(a,(k-1))){
      ind <- i
      
      # Get data for f, P, and S for each of the seasons/classes
      for (j in 1:NUMNET){
        type <- j
        f_update[[j]] <- f_function()
        p_update[[j]] <- p_function()
        s_update[[j]] <- s_function()
      }
      
      #transition probability
      p[1:nSites,1:nSites,ind]<-as.matrix(p_update[[sex]])
      
      #Adult edge survival
      sA_edge[1:nSites,1:nSites,ind]<-as.matrix(s_update[[sex]])
      
      #Juvenile edge survival
      if(ind == breedSeason){
        sJ_edge[1:nSites,1:nSites,ind]<-as.matrix(s_update[[sex+2]])
      }
      else{
        sJ_edge[1:nSites,1:nSites,ind]<-sA_edge[1:nSites,1:nSites,ind]
      }
      
      #Adult node survival and Reproduction
      for (j in 1:num_nodes){
        if((unlist(N[[t]])[(sex-1)*nSites+j]+unlist(N[[t]])[(sex+1)*nSites+j])!=0){  #if (sex-specific) node pop is not zero
          sA_node[j,ind] <- f_update[[sex]][j]/(unlist(N[[t]])[(sex-1)*nSites+j]+unlist(N[[t]])[(sex+1)*nSites+j])
        }
        if(unlist(N[[t]])[j]!=0 && ind == breedSeason){  #if adult females pop is not zero and it's the breeding season
          R0[j,ind] <- f_update[[sex+2]][j]/(unlist(N[[t]])[j]*sA_node[j,ind]) 
        }
      }
      #Juvenile node survival
      sJ_node[1:nSites,ind] <- sA_node[1:nSites,ind] 
      
      t = t+1 
    }
    
  }
  
  ## Cr for each season ####
  for (k in a-1){  
    CrprodA <- diag(nSites)  #initialze with identity matrix
    CrprodJ <- diag(nSites)  
    COIprodA <- diag(nSites)
    COIprodJ <- diag(nSites)
    seasonvec <- shifter(a, k) #start at season (k+1) as the anniversary date
    breedOccurred <- 0  #breeding season has not occurred yet
    for (i in seasonvec){  
      sA_node_mat <- diag(sA_node[,i]) # matrix with adult node survival as diagonal elements
      qA <- as.matrix(sA_edge[,,i]*p[,,i]) # Hadamard product of Adult edge survival and probability
      if(i == breedSeason){  #Check if the breeding season occurred and define Juvenile matrix accordingly
        breedOccurred <- 1
        sJ_node_mat <- diag(sA_node[,i]*R0[,i]) # matrix with adult node survival times reproduction as diagonal elements
        sJ <- as.matrix(sJ_edge[,,i]) # Juvenile edge survival
      }
      else if(breedOccurred == 1){
        sJ_node_mat <- diag(sJ_node[,i]) # matrix with juvenile node survival as diagonal elements
        sJ <- as.matrix(sJ_edge[,,i]) # Juvenile edge survival
      }
      else {
        sJ_node_mat <- sA_node_mat  # matrix with adult node survival as diagonal elements
        sJ <- as.matrix(sA_edge[,,i]) # adult edge survival
      }
      CrprodA <- CrprodA %*% sA_node_mat %*% qA  # matrix multiplication
      CrprodJ <- CrprodJ %*% sJ_node_mat %*% as.matrix(sJ*p[,,i]) 
      if (i!=seasonvec[length(seasonvec)]){  #if not the last season
        COIprodA <- COIprodA %*% sA_node_mat %*% sA_edge[,,i]
        COIprodJ <- COIprodJ %*% sJ_node_mat %*% sJ 
      }
    }
    CR[,k+1] <- (CrprodA + CrprodJ) %*% ones  #column vector of Cr values for time step k+1
    COI[,,k+1] <- as.matrix(COIprodA*(ones %*% t(sA_node_mat %*% qA %*% ones))) + as.matrix(COIprodJ*(ones %*% t(sJ_node_mat %*% as.matrix(sJ*p[,,i]) %*% ones))) #matrix of Coi values for time step k+1
  }
  # return ####
  return(CR)
} # end Cr_fn
