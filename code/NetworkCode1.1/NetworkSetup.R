#######################################
##         NETWORK SETUP             ##
#######################################

## This code sets up the network variables to be used in the Network Simulation.
## All data should be stored in .xlsx spreadsheets in the correct format.
## Spreadsheets should be located at ./SIMNAME/network_inputs_NETNAME.xlsx
## Where SIMNAME and NETNAME are defined in the SpeciesSimulation.R script

## USERS SHOULD NOT NEED TO INTERACT WITH THIS CODE

nodes<-list()
alpha<-list()
beta<-list()
N_initial<-list()

# Find the number of classes
NUMNET <- length(NETNAME)

# Set the input file names
input_file_names <- matrix(0,1,NUMNET)
for (i in 1:NUMNET){
  input_file_names[i]<-c(paste(SIMNAME,"/network_inputs_",NETNAME[i],".xlsx", sep=""))
}

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


## Find the number of node variables
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


## SET UP THE NETWORK(S) ##

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
  
  ## Read in the remaing worksheets for each season
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
    
    
  }

  
  ## Store the node and path characteristics in lists
  alpha[[networkname]] <- NODE
  beta[[networkname]] <- PATH
  
  rm(NODE,EDGEPROB, EDGESURVIVE, PATH)
  
}


### CHECK THAT EACH NETWORK IS THE SAME SIZE ###
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
rm(list=setdiff(ls(),c(network_variables,base_variables)))

print("Network Setup Complete")

