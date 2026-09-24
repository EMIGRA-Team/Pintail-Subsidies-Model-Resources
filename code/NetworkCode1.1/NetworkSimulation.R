#######################################
##           SIMULATIONS             ##
#######################################

## This code runs the network simulation based on the general network model
## It first loads the user defined Newtork Functions: f_(i,t), p_(ij,t), and s_(ij,t)

## USERS SHOULD NOT NEED TO INTERACT WITH THIS CODE


source("SpeciesFunctions.R")

N <- list()
M <- list()
WR <-list()
total_pop <- list()
f_update <- list()
p_update <- list()
s_update <- list()


## INITIALIZE NODE POPULATION
N[[1]] <- N_initial # Set initial population after movement to each node (before reproduction/survival at node)

### INITIALIZE OTHER POPULATION DATA
M[[1]] <- vector("list", length(N_initial)) # Path Population Matrix
names(M[[1]]) <- NETNAME
total_pop[[1]] <- vector("list", length(N_initial)) # total network population - sum of population at each node
names(total_pop[[1]]) <- NETNAME
WR[[1]] <- vector("list", length(N_initial)) # population distribution at each node
names(WR[[1]]) <- NETNAME


for (i in 1:NUMNET){
  total_pop[[1]][i] <- lapply(N[[1]][i], function(x) sum(x))
  test <- as.double(total_pop[[1]][i])
  if (test == 0){WR[[1]][i] <- N[[1]][i]}
  else{WR[[1]][i] <- lapply(N[[1]][i],y=unlist(total_pop[[1]][i]), function(x,y) x/y)}
}
rm(i)

if(SILENT==F){
  print("Initial Population")
  print(total_pop[[1]])
}

### INITIALIZE ERROR ###
errorstop <- 0
ERRPOP_OLD <- matrix(100,1,seasons)  

### INITIALIZE TIME STEPS ###
t <- 0

### START SIMULATION ###
### LOOP THROUGH TIME ###
while (errorstop == 0){
  t <- t+1
  if(SILENT==F){
    print("Time Step")
    print(t)
  }
  
  ind <- ((t-1)%%seasons) +1 # season number
  if(SILENT==F){
    print("Season Number")
    print(ind)
  }
  
  ## MODEL FUNCTIONS ##
  for (i in 1:NUMNET){
    type <- i
    f_update[[i]] <- f_function()   # f_function(N,alpha,i,ind,t) # Number of individuals leaving each node
    }
  for (i in 1:NUMNET){
    type <- i
    p_update[[i]] <- p_function()    # p_function(N,f_update,alpha,p_edge,beta,i,ind,t) # Path transition probability
    s_update[[i]] <- s_function()   # s_function(N,f_update,alpha,s_edge,beta,i,ind,t) # Path survival probability
  }
  rm(i)
  
  if(SILENT==F){
    print("f_(i,t)=")
    print(f_update)
    print("p_(ij,t)=")
    print(p_update)
    print("s_(ij,t)=")
    print(s_update)
  }
  
  ## MODEL EQUATION ##
  ## Create lists elements for next time step
  M[[t]] <- vector("list", length(N_initial))
  names(M[[t]]) <- NETNAME  # originally names(M[[1]]) <- NETNAME , changed by BM
  N[[t+1]] <- vector("list", length(N_initial))
  names(N[[t+1]]) <- NETNAME
  
  ## Calculate next time step; c(s_update,p_update,f_update)
  for (i in 1:NUMNET){
    # skip list if null -- added by BM during troubleshooting
    if (length(s_update[[i]]) == 0L ||
        length(p_update[[i]]) == 0L ||
        length(f_update[[i]]) == 0L) {
      next
    }
    
    M[[t]][i] <- list(s_update[[i]]*p_update[[i]]*f_update[[i]]) # Number of individuals on each path
    N[[t+1]][i] <- list(colSums(M[[t]][[i]])) # Number of individuals arriving at the nodes in the next season
  }
  rm(i)
  
  ## UPDATE TOTAL POPULATION ##
  total_pop[[t+1]] <- vector("list", length(N_initial))
  names(total_pop[[t+1]]) <- NETNAME
  
  WR[[t+1]] <- vector("list", length(N_initial))
  names(WR[[t+1]]) <- NETNAME
  
  ## Set total population data
  for (i in 1:NUMNET){
    total_pop[[t+1]][i] <- lapply(N[[t+1]][i], function(x) sum(x))
    test <- as.double(total_pop[[t+1]][i])
    if (test == 0){WR[[t+1]][i] <- N[[t+1]][i]}
    else{WR[[t+1]][i] <- lapply(N[[t+1]][i],y=unlist(total_pop[[t+1]][i]), function(x,y) x/y)}
  }
  
  if(SILENT==F){
    print(paste("Total Population in season", ind, "for time step", t))
    print(total_pop[t+1])
  }
  if(SILENT==F){
    print(paste("Node Population in season", ind, "for time step", t))
    print(N[t+1])
  }
  
  ### TEST FOR BLOW-UP OR INFINITE NUMBERS ###
  if(any(lapply(total_pop[[t+1]], function(x) is.finite(as.double(x)))=="FALSE")){
    stop("\n NaN found in population data: \n *** This means an infinite population has been reached in at least one node. \n *** Check divide by zero or missing data in model parameters. \n *** Check density dependent equations. \n *** This is likely caused by SpeciesFunctions.R")}
  
  
  ### CALCULATE THE ERROR AT EACH SEASON ###
  if(t >= seasons){
    sum_new <- 0
    sum_old <- 0
    for (i in 1:NUMNET){
      sum_new <- sum_new + unlist(total_pop[[t+1]][i])  
      sum_old <- sum_old + unlist(total_pop[[t+1-seasons]][i])
    }
    ERRPOP_OLD[1,ind] <- abs(sum_new - sum_old)
    ## Only stop if the total error is less than the allowable error across all seasons
    if(all(ERRPOP_OLD < ERR)){errorstop <- 1}
    if(t+1 >= tmax){
      errorstop <- 1 
      print("\n The simulation did not converge within the maximum time allowed")}
  }
}

# We give results for the total population at the start of season 1
# Check at which season the simulation stopped
timestep <- t+1 - ind%%seasons

# Clear the workspace reserving needed network input variables and base variables and simulation variables
simulation_variables <- c("timestep","WR","total_pop","M","N","t","ind","f_update","p_update","s_update","simulation_variables")
rm(list=setdiff(ls(),c(network_variables,base_variables,simulation_variables)))


