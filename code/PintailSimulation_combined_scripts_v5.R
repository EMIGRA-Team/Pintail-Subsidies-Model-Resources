# Front matter #### 
# Required libraries are loaded in ecosystem_service_flows.r
#  Originally developed by Joanna Bieri and Christine Sample,
#  B. Mattsson made the following changes:  1) combined all subscripts (originally sourced) into this script except for functions
#                                             a) removed density-dependent reproduction within breeding areas via f_function
#                                             b) functions are now in population_model_functions.r, sourced once from ecosystem_service_flows.r
#                                           2) converted into function then to be called by ecosystem_service_flows.r within R project 
#                                           3) accordingly updated & streamlined headings for easier navigation

# Set the working directory, only needed if working outside of R project containing required folders and scripts in main folder 
# -> may need additional troubleshooting to run outside such an R project
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

# clear workspace except for chosen objects and functions
#rm(list = ls()) # removes all objects
keep <- c("scenarioName", "scenarios", "regions", "regionResults_list", "ES_flows_array")
rm(list = setdiff(ls(), c(keep, lsf.str())))

# SET UP PINTAIL SPECIFIC NETWORK ####

# IMPORTANT NOTE #
# Users should specify network model functions f_(i,t), p_(ij,t), and s_(ij,t) below, even if these are constants they must still be set as such.

### User specified information specific to the simulation ###

SIMNAME <- scenarioName # Specifies which baseline folder, will be a function argument, during development specify in ecosystem_service_flows.r
# This is the subfoulder where the network data, outputs, and .RData file should are saved
# This is also the subfoulder where the network input files are stored: 
#   "./SIMNAME/network_inputs_NETNAME.xlsx

NETNAME <- c("adult_female", "adult_male", "juvenile_female", "juvenile_male") # Give a distinct name for each class as used in input files
# Order is important here we would index [[1]] = class 1 and [[2]] = class 2 in alpha and beta.

ERR <- 1 # Error tolerance for convergence. 
# To test convergence, we compare total population of all classes in the current season
# to the matching season from the previous year.

seasons <- 3 # Number of seasons or steps in one annul cycle. 
# This must match number of spreadsheets in input files

num_nodes <- 5 # Number of nodes in the network
# This must match the number of initial conditions given in input files

tmax <- 301 # Maximum number of steps to take - assume non convergence if t=tmax

OUTPUTS <- TRUE # TRUE = Process final outputs, FALSE = Do not process just run the sumulation.

## For debugging your model equations ##
SILENT <- TRUE # TRUE = Do not print data to console - silence outputs.
# FALSE = Print population data and network function data to the Console for debugging.

# This code sets up the network variables to be used in the Network Simulation.
# All data should be stored in .xlsx spreadsheets in the correct format.
# Spreadsheets should be located at ./SIMNAME/network_inputs_NETNAME.xlsx
# Where SIMNAME and NETNAME are defined in the SpeciesSimulation.R script

# Set location of source code if working outside R project
#netcode <- c("../NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Set location of source code if working in R project
netcode <- c("NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Clear the workspace reserving needed network input variables
base_variables <- c("seasons", "num_nodes", "NETNAME", "tmax", "SIMNAME", "ERR", "OUTPUTS", "SILENT","netcode","base_variables", "scenarioName")
#rm(list=setdiff(ls(), base_variables))

#source(paste(netcode,"NetworkSetup.R",sep="")) # should finish running in a few seconds

nodes<-list()
alpha<-list()
beta<-list()
N_initial<-list()

# Find the number of classes
NUMNET <- length(NETNAME)
type <- 1 # use network 1 (adult females) for troubleshooting

# Set the input file names
input_file_names <- matrix(0,1,NUMNET)
for (i in 1:NUMNET){
  input_file_names[i]<-c(paste(SIMNAME,"/network_inputs_",NETNAME[i],".xlsx", sep=""))
}
# start dependency on XLConnect ####
# Check the validity of input files
for (i in 1:NUMNET){
  CHECK_WORKBOOK <- file.exists(input_file_names[i])
  if (CHECK_WORKBOOK==F){stop(paste("The Workbook,", input_file_names[i],"could not be found. \n *** Please check that the end of the input file names match NETNAME"))}
}
for (i in 1:NUMNET){
  SPREADSHEETS <- getSheets(loadWorkbook(input_file_names[i]))
  NUMSHEETS <- length(SPREADSHEETS)
  EXPECTEDSHEETS <- seasons + 1
  if (NUMSHEETS != EXPECTEDSHEETS){stop(paste("\n The Workbook,", input_file_names[i],"has an incorrect number of sheets. \n *** It should contain one sheet for initial conditions and one sheet for each season."))}
}


## Find the number of node variables ####
DATA <- readWorksheetFromFile(input_file_names[1],sheet=2)
varname_node <-list()
i=3
j=1

test <- DATA[1,i]=="STOP"
while(test == F){
  varname_node[[j]]<-DATA[1,i]
  j = j+1
  i = i+1
  test <- DATA[1,i]=="STOP"
}
numvar_node <- length(varname_node)

## Find the number of path variables
varname_path <-list(c("p_ij"),c("s_ij"))
i=1+3*(num_nodes+3) # First location after p_ij and s_ij
j=1

test <- DATA[i,1]=="STOP"
while(test == F){
  varname_path[[j+2]]<-DATA[i,1]
  j = j+1
  i = i+num_nodes+3
  test <- DATA[i,1]=="STOP"
}
numvar_path <- length(varname_path)-2


## SET UP THE NETWORK(S) ####

for (i in 1:NUMNET){
  networkname <- NETNAME[i]
  NODE <- list()
  EDGEPROB <- list()
  EDGESURVIVE <- list()
  PATH <- list()
  
  ## Read in first worksheet
  ## The first worksheet should contain initial conditions 
  ## Data should start in cell B2
  Ninit <- readWorksheetFromFile(input_file_names[i], sheet=1, startRow=1)
  N_initial[[networkname]] <- Ninit[,2]
  n <- nrow(Ninit) #Number of Nodes
  
  if((n != num_nodes) | (any(is.na(Ninit[,2])))){
    stop(paste("\n Incorrect number of initial conditions given for species:", networkname,"\n Check you input files."))}
  
  nodes[[i]]<-n
  
  ## Read in the remaing worksheets for each season ####
  ## t=0 season should be worksheet 2
  ## Node data belongs on the top starting in cell C2
  ## Path data should be contained in nXn ranges below the node data with 3 rows separating
  ## Transition rates should start in cell C(n+5) where n=number of nodes
  ## Survival probabilities should start in cell C(2n+9)
  ## Other path variables should start in cell C(3n+12)
  for (k in 1:seasons){
    # READ IN THE DATA
    SR <- 2 # row in the spreadsheet where we start reading data
    SC <- 3 # column in the spreadsheet where we start reading data
    ECnode <- SC + numvar_node - 1 # column in the spreadsheet where we stop reading node data
    ECpath <- SC + n -1
    
    ###____seasonal node characteristics ____###
    NODE[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECnode)
    
    # Check that there are no empty cells in node characteristics
    if(any(is.na(NODE[[k]]))){
      stop(paste('\n Check the input file! \n *** There is at least one empty node characteristic in season', k))}
    
    ###____seasonal edge characteristics____###
    path_vars <- vector("list", length(numvar_path))
    
    SR <- (SR + n + 3)
    EDGEPROB[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
    path_vars[[1]] <- EDGEPROB[[k]]
    
    SR <- (SR + n + 3)
    EDGESURVIVE[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
    path_vars[[2]] <- EDGESURVIVE[[k]]
    
    # Check that there are no empty cells in edge transition and survival probabilities
    if((any(is.na(EDGEPROB[[k]]))) | (any(is.na(EDGESURVIVE[[k]])))){
      stop(paste('\n Check the input file! \n *** There is at least one empty edge transition or survival in season', k))}
    
    # Check the sum of transition probabilities should sum to 1 or 0
    x <- unlist(lapply(EDGEPROB[k],rowSums))
    eps <- 0.000001
    if( any(x < 1-eps & x!=0) | any(x > 1+eps & x!=0) ){ stop('\n Check the input file! \n *** Edge transition probabilities must sum to 0 or 1')}
    
    if(numvar_path > 0){
      for(j in 1:numvar_path){
        SR <- (SR + n + 3)
        path_vars[[j+2]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
      }
    }
    PATH[[k]] <- path_vars
    rm(path_vars)
    names(PATH[[k]]) <- varname_path
  }  # end for (k in 1:seasons)

  ## Store the node and path characteristics in lists
  alpha[[networkname]] <- NODE
  beta[[networkname]] <- PATH
  
  rm(NODE,EDGEPROB, EDGESURVIVE, PATH)
} # end for (i in 1:NUMNET)

# end dependency on XLConnect #### 

### CHECK THAT EACH NETWORK IS THE SAME SIZE ####
test1 <- nodes[[1]]
for(i in 1:NUMNET){
  test2 <- nodes[[i]]
  if( test1!=test2 ) stop('\n Check that the networks have the same number of nodes.')
}
### STORE NUMBER OF NODES ###
nSites <- test2

### CHECK THAT THE NUMBER OF PATH VARIABLES MATCHES NUMBER OF NODES ###
for (i in 1:NUMNET){
  for (j in 1:seasons){
    for (k in 1:length(varname_path)){
      check1 <- ncol(beta[[i]][[j]][[k]])
      check2 <- nrow(beta[[i]][[j]][[k]])
      if(check1 != check2){stop(paste("\n", varname_path[[k]], "for class", NUMNET[[1]], "season", j, "should be a square matrix. \n Check your input files"))}
      if(check1 != n){stop(paste("\n", varname_path[[k]], "for class", NUMNET[[1]], "season", j, "should be an nXn where n=number of nodes. \n Check you input files"))}
    }
  }
}

# Clear the workspace reserving needed network input variables and base variables
network_variables <- c("N_initial","alpha","p_edge","s_edge","beta","varname_path","varname_node","NUMNET","network_variables")
#rm(list=setdiff(ls(),c(network_variables,base_variables)))

print("Network Setup Complete")

# POPULATION NETWORK SIMULATION ####
#print(paste("Running", SIMNAME, sep=" "))
#source(paste(netcode,"NetworkSimulation.R",sep=""))

## This code runs the network simulation based on the general network model
## It first loads the user defined Newtork Functions: f_(i,t), p_(ij,t), and s_(ij,t)

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

# Clear the workspace reserving functions and needed network input variables and base variables and simulation variables
simulation_variables <- c("timestep","WR","total_pop","M","N","t","ind","f_update","p_update","s_update","simulation_variables")
base_variables <- c("nSites", "seasons", "num_nodes", "NETNAME", "tmax", "SIMNAME", "ERR", "OUTPUTS", "SILENT","netcode","base_variables","OUTPUTNAME", "SYSDATE","NODENAMES")
#rm(list=setdiff(ls(),c("scenarioName", network_variables,base_variables,simulation_variables,lsf.str())))
  
# PROCESS NETWORK OUTPUTS ####
# if (OUTPUTS == T){  source(paste(netcode,"NetworkOutputs.R",sep="")) }

## This code produces basic outputs for the Network simulation.
## 1. A graph of Population over time for each class - to show each population reaching a steady state.
## 2. A table of values showing the steady state total network populations for each season at steady state.
## 3. A single season graph of the steady state total network populations for each season at steady state.

## ggplot of annual changes in total breeding population  ####
# Helper to safely extract a single numeric component
get_component <- function(x, name) {
  if (!is.null(x[[name]]) && length(x[[name]]) >= 1) as.numeric(x[[name]][1]) else NA_real_
}

# Extract cohorts and compute total per time step
af <- sapply(total_pop, get_component, name = "adult_female")
am <- sapply(total_pop, get_component, name = "adult_male")
jf <- sapply(total_pop, get_component, name = "juvenile_female")
jm <- sapply(total_pop, get_component, name = "juvenile_male")

pop_total <- af + am + jf + jm

df_yearly <- tibble(
  t = seq_along(pop_total),
  pop_total = pop_total
) %>%
  filter(t %% seasons == 0) %>%           # keep only the last season in each year
  mutate(year = t / seasons) %>%          # convert time step to year number
  filter(year <= 300)                     # cap at 300 years (if available)

# ggplot(df_yearly, aes(x = year, y = pop_total)) +
#   geom_line(color = "steelblue", linewidth = 1) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Total population by year (every 3rd time step)",
#     x = "Year",
#     y = "Total count"
#   ) +
#   theme_minimal()
## probably not needed: Base-R Plot total populations over time  ####
# platform <- .Platform$OS.type
# if(platform == "unix") {X11()
# } else{
#   if(platform == "windows"){windows()
#   } else{quartz()}}

time_steps <- 0:timestep/seasons
STEPwidth <- seasons
STEPstart <- 1
graphbot <- 0 
graphtop <- max(unlist(lapply(total_pop[c(seq(STEPstart, timestep+1,by=STEPwidth))], function(x) x[])))
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20
total_pop_plot <- data.frame(matrix(0,length(N),NUMNET))

# plot(c(0),c(0),type="l", 
#      main=paste("Total Network Population vs. Time \n Single Annual Survey (beginning of season 1)"),
#      ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
#      xlab="Years",ylab="Total Population")
# 
# xplot <- time_steps[seq(STEPstart, length(time_steps),by=STEPwidth)]
# for(i in 1:NUMNET){
#   par(new=TRUE)
#   
#   # head(total_pop)
#   total_pop_plot[,i] <- unlist(lapply(total_pop, function(x) x[][[i]]))
#   
#   yplot <- total_pop_plot[seq(STEPstart, length(time_steps),by=STEPwidth),i]
#   dot <- as.double(dottype[i])
#   plot(xplot,yplot,type="o", lty=1, pch=dot,ylim = c(graphbot,graphtop), 
#        xlim = c(min(time_steps),max(time_steps)), axes="FALSE", xlab = "", ylab = "")
#   dottype[i+1] <- dottype[i] + 1
# }
# rm(i)
# 
# legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)


## Store data as .csv for steady state annual cycle population numbers  ####  
pop_output <- data.frame(matrix(0,seasons,NUMNET))
for(i in 1:NUMNET){
  temp <- unlist(lapply(total_pop, function(x) x[][[i]]))
  pop_output[,i] <- data.frame(temp[seq(timestep-seasons+1,timestep, by=1)],row.names=NULL) 
  
}
rm(i) 

colnames(pop_output) <- NETNAME
for(i in 1:seasons){
  rownames(pop_output)[i] <- paste("season",i)}
pop_output_sum <- pop_output %>% rowwise() %>%   mutate(total = sum(c_across(everything()), na.rm = TRUE)) %>%  ungroup()
# print("Total Equilibrium Population - for each season")
# print(pop_output_sum)
write.csv(pop_output_sum,file=paste(SIMNAME,"/SteadyStatePopulation.csv",sep=""))
rm(i) # pop_output_sums <- pop_output %>% rowwise() %>%   mutate(total = sum(c_across(everything()), na.rm = TRUE)) %>%  ungroup()


## Plot steady state annual cycle population numbers ####
# platform <- .Platform$OS.type
# if(platform == "unix"){X11()
# } else{
#   if(platform == "windows"){windows()}
#   else{quartz()}}

time_steps <- 1:seasons
graphbot <- 0 
graphtop <- max(pop_output)
rm(dottype)
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20

# plot(c(0),c(0),type="l", ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
#      xaxt = "n", main=paste("Class Population at Steady State \n Over One Annual Cycle"),
#      xlab="Season Number",ylab="Population")
# 
# axis(side = 1, at = time_steps)
# for(i in 1:NUMNET){
#   par(new=TRUE)
#   dot <- as.double(dottype[i])
#   plot(time_steps,pop_output[,i],type="o", lty=1, pch=dot, ylim = c(graphbot,graphtop), 
#        xlim = c(min(time_steps),max(time_steps)), axes = FALSE, xlab = "", ylab = "")
#   dottype[i+1] <- dottype[i] + 1
# }
# rm(i)
# legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)

# Clear the workspace reserving functions and needed network input variables and base variables and simulation variables
output_variables <- c("pop_output", "output_variables")
#rm(list=setdiff(ls(),c(keep, network_variables,base_variables,simulation_variables,output_variables, lsf.str())))

# Calculate Ds  ####

WEIGHT <- 0
numSex <- 2 # Calculate Cr separately for males and females
DSind <- array(0,c(num_nodes,seasons,numSex))

#Choose which type (male or female) of adult to calculate Cr

for (sex in 1:numSex){
  
  # Get Cr Values ####
  #source("Cr_pintail.R")
  #CR <- Cr_fn(seasons, nSites, num_nodes, timestep, NUMNET, sex, N)
  ## Front matter ####
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
  
  ## Initialize ####
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
  
  ## Calculate Cr ####
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
  # Troubleshoot NAs in CrprodJ
  i_names <- paste0("i_", c(2, 3, 1)) # Desired i dimnames order
  
  # 2D list-matrix: rows = k in a-1 (0,1,2), cols = i in order 2,3,1
  CrprodJ_check <- matrix(
    vector("list", seasons * seasons),
    nrow = seasons, ncol = seasons,
    dimnames = list(
      k = paste0("k_", a - 1),
      i = i_names
    )
  ) # end array definition of CrprodJ_check
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
      } else if(breedOccurred == 1){
        sJ_node_mat <- diag(sJ_node[,i]) # matrix with juvenile node survival as diagonal elements
        sJ <- as.matrix(sJ_edge[,,i]) # Juvenile edge survival
      } else {
        sJ_node_mat <- sA_node_mat  # matrix with adult node survival as diagonal elements
        sJ <- as.matrix(sA_edge[,,i]) # adult edge survival
      }
      CrprodA <- CrprodA %*% sA_node_mat %*% qA  # matrix multiplication
      CrprodJ <- CrprodJ %*% sJ_node_mat %*% as.matrix(sJ*p[,,i])
      if (i!=seasonvec[length(seasonvec)]){  #if not the last season
        COIprodA <- COIprodA %*% sA_node_mat %*% sA_edge[,,i]
        COIprodJ <- COIprodJ %*% sJ_node_mat %*% sJ
      }
      CrprodJ_check[i][k] <- CrprodJ
      #print(lst(k,i,CrprodJ))
    } # end for (i in seasonvec)
    # BM: replacing NAs with 0s to avoid NAs in Ds
    CrprodJ_z <- CrprodJ
    CrprodJ_z[is.na(CrprodJ_z)] <- 0
    CR[, k + 1] <- (CrprodA + CrprodJ_z) %*% ones
    #CR[,k+1] <- (CrprodA + CrprodJ) %*% ones  #column vector of Cr values for time step k+1
    COI[,,k+1] <- as.matrix(COIprodA*(ones %*% t(sA_node_mat %*% qA %*% ones))) + as.matrix(COIprodJ*(ones %*% t(sJ_node_mat %*% as.matrix(sJ*p[,,i]) %*% ones))) #matrix of Coi values for time step k+1
  }
  
  
  # Ds ####
  start <- timestep-seasons
  end <- timestep-1
  CrCount <- 1
  for(t in (start:end)){
    if(sex==1){##Females
      POP<- matrix(unlist((N[[t]]$adult_female)))+ matrix(unlist((N[[t]]$juvenile_female)))
    }
    if(sex==2){##Males
      POP<- matrix(unlist((N[[t]]$adult_male)))+ matrix(unlist((N[[t]]$juvenile_male)))
    }
    
    for(i in (1:num_nodes)){
      # Unweighted Ds Values Cr * Population
      
      DSind[i,CrCount,sex] <- CR[i,CrCount]*POP[i]   ### CR is a list derived from Cr_pintail code
    }
    CrCount <- CrCount + 1
  }
  # Weighing factor so final counts sum to one
  WEIGHT <- WEIGHT + colSums(DSind[,,sex])
  
  # Region names
  region_names <- c("Alaska breeding", "Southern breeding", "Northern breeding", "Western wintering", "Central wintering")
  
  # Equilibrium population at final timestep
  HARVEST_TIME <- timestep - 2
  
  if(sex == 1){
    
    ADULT_REGION <- unlist(N[[HARVEST_TIME]]$adult_female)
    JUVENILE_REGION <- unlist(N[[HARVEST_TIME]]$juvenile_female)
    
  }
  
  if(sex == 2){
    
    ADULT_REGION <- unlist(N[[HARVEST_TIME]]$adult_male)
    JUVENILE_REGION <- unlist(N[[HARVEST_TIME]]$juvenile_male)
    
  }
  
  EQ_POP_REGION <- ADULT_REGION + JUVENILE_REGION
  EQ_POP_TOTAL <- sum(EQ_POP_REGION)
  
  HARVEST_RATE <- 0
  
  if(grepl("Conservative", SIMNAME)){
    HARVEST_RATE <- 0.05  
  }
  
  if(grepl("Grassland", SIMNAME)){
    HARVEST_RATE <- 0.05  
  }
  
  if(grepl("Hunting", SIMNAME)){
    HARVEST_RATE <- 0
  }
  
  if(grepl("Intermediate", SIMNAME)){
    HARVEST_RATE <- 0
  }
  
  if(sex == 1){   # females
    
    HARVESTED_REGION <-
      ADULT_REGION * HARVEST_RATE * 1.00 +
      JUVENILE_REGION * HARVEST_RATE * 2.00
    
  }
  
  if(sex == 2){   # males
    
    HARVESTED_REGION <-
      ADULT_REGION * HARVEST_RATE * 1.25 +
      JUVENILE_REGION * HARVEST_RATE * 2.75
    
  }
  
  HARVESTED_TOTAL <- sum(HARVESTED_REGION)
  
  # We save CR y POP values for each season (CrCount / k)
  for(season_k in 1:seasons) {
    formula_report <- data.frame(
      Scenario       = SIMNAME,
      Sex_Index      = sex,
      Sex_Name     = ifelse(sex == 1, "Females", "Males"),
      Season        = season_k,
      Region          = region_names,
      CR_Formula_Pure = as.numeric(CR[, season_k]),
      POP_Population   = as.numeric(DSind[, season_k, sex] / ifelse(CR[, season_k] == 0, 1, CR[, season_k])),
      DSind_Result = as.numeric(DSind[, season_k, sex]),
      Equilibrium_POP_Region = as.numeric(EQ_POP_REGION),
      Equilibrium_POP_Total = EQ_POP_TOTAL,
      Harvest_Rate = HARVEST_RATE,
      Harvested_Individuals_Region = as.numeric(HARVESTED_REGION),
      Harvested_Individuals_Total = HARVESTED_TOTAL,
      Adult_Population    = as.numeric(ADULT_REGION),
      Juvenile_Population = as.numeric(JUVENILE_REGION),
      Harvested_Adults = as.numeric(ADULT_REGION * HARVEST_RATE * ifelse(sex == 1, 1.00, 1.25)),
      Harvested_Juveniles = as.numeric(JUVENILE_REGION * HARVEST_RATE * ifelse(sex == 1, 2.00, 2.75)),
      
      stringsAsFactors = FALSE
    )
    
    # Temporal file to register the values
    filepintail <- paste0("bagstad_raw_", SIMNAME, "_sex_", sex, "_est_", season_k, ".csv")
    write.csv(formula_report, filepintail, row.names = FALSE)
  }
  ## write population size at start of each season in each region at end of simulation ####
  # v7
  create_season_vars_fn <- function(N, ind) {
    n <- length(N); idxs <- (n-2):n
    seasons <- ((idxs - 1) %% 3) + 1
    totals_one <- function(elem) {
      regs <- names(elem[[1]])
      sapply(regs, function(r) sum(sapply(elem, function(v) v[r]), na.rm = TRUE))
    }
    vals <- unlist(Map(function(elem, s) {
      t <- totals_one(elem)
      setNames(t, paste0(names(t), "_season_", s))
    }, N[idxs], seasons), use.names = TRUE)
    list2env(as.list(vals), envir = .GlobalEnv)
    invisible(vals)
  }
  season_vars_df <- create_season_vars_fn(N,ind)
  write.csv(season_vars_df,paste('season_vars_df',SIMNAME, '.csv', sep='_'))
  
} 


# cat("Seasonal Proportional Dependence:\n Female: \n")
# cat("     Breeding   Winter   Spring Stop\n")
# print(DSind[,,1])
# 
# cat("Seasonal Proportional Dependence:\n Male: \n")
# cat("     Breeding   Winter   Spring Stop\n")
# print(DSind[,,2])
# 
# 
# cat("\n\n Annual Average Proportional Dependence:\n")
  DS <- matrix(rowSums(DSind)/3,nrow=num_nodes)
# print(DS)
# 
# cat("\n\n Steady state population:\n")
# print(pop_output_sum)


# SAVE THE DATA and outputs ####
#save.image(file=paste(SIMNAME,"/",SIMNAME, ".RData", sep = ""))

