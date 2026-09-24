#################################
##        BASIC OUTPUTS        ##
#################################

## This code produces basic outputs for the Network simulation.
## 1. A graph of Population over time for each class - to show each population reaching a steady state.
## 2. A table of values showing the steady state total network populations for each season at steady state.
## 3. A single season graph of the steady state total network populations for each season at steady state.

## USERS SHOULD NOT NEED TO INTERACT WITH THIS CODE

#### Plot total populations over time  ####
platform <- .Platform$OS.type
if(platform == "unix") {X11()
} else{
  if(platform == "windows"){windows()
  } else{quartz()}}

time_steps <- 0:timestep/seasons
STEPwidth <- seasons
STEPstart <- 1
graphbot <- 0 
graphtop <- max(unlist(lapply(total_pop[c(seq(STEPstart, timestep+1,by=STEPwidth))], function(x) x[])))
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20
total_pop_plot <- data.frame(matrix(0,length(N),NUMNET))

plot(c(0),c(0),type="l", 
     main=paste("Total Network Population vs. Time \n Single Annual Survey (beginning of season 1)"),
     ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
     xlab="Years",ylab="Total Population")

xplot <- time_steps[seq(STEPstart, length(time_steps),by=STEPwidth)]
for(i in 1:NUMNET){
  par(new=TRUE)
  
  total_pop_plot[,i] <- unlist(lapply(total_pop, function(x) x[][[i]]))
  
  yplot <- total_pop_plot[seq(STEPstart, length(time_steps),by=STEPwidth),i]
  dot <- as.double(dottype[i])
  plot(xplot,yplot,type="o", lty=1, pch=dot,ylim = c(graphbot,graphtop), 
       xlim = c(min(time_steps),max(time_steps)), axes="FALSE", xlab = "", ylab = "")
  dottype[i+1] <- dottype[i] + 1
}
rm(i)

legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)


#### Store data as .csv for steady state annual cycle population numbers  ####  
pop_output <- data.frame(matrix(0,seasons,NUMNET))
for(i in 1:NUMNET){
  temp <- unlist(lapply(total_pop, function(x) x[][[i]]))
  pop_output[,i] <- data.frame(temp[seq(timestep-seasons+1,timestep, by=1)],row.names=NULL) 
  
}
rm(i)

colnames(pop_output) <- NETNAME
for(i in 1:seasons){
  rownames(pop_output)[i] <- paste("season",i)}
print("Total Equilibrium Population - for each season")
print(pop_output)
write.csv(pop_output,file=paste(SIMNAME,"/SteadyStatePopulation.csv",sep=""))
rm(i)


#### Plot steady state annual cycle population numbers ####
platform <- .Platform$OS.type
if(platform == "unix"){X11()
} else{
  if(platform == "windows"){windows()}
  else{quartz()}}

time_steps <- 1:seasons
graphbot <- 0 
graphtop <- max(pop_output)
rm(dottype)
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20

plot(c(0),c(0),type="l", ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
     xaxt = "n", main=paste("Class Population at Steady State \n Over One Annual Cycle"),
     xlab="Season Number",ylab="Population")

axis(side = 1, at = time_steps)
for(i in 1:NUMNET){
  par(new=TRUE)
  dot <- as.double(dottype[i])
  plot(time_steps,pop_output[,i],type="o", lty=1, pch=dot, ylim = c(graphbot,graphtop), 
       xlim = c(min(time_steps),max(time_steps)), axes = FALSE, xlab = "", ylab = "")
  dottype[i+1] <- dottype[i] + 1
}
rm(i)
legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)


# Clear the workspace reserving needed network input variables and base variables and simulation variables
output_variables <- c("pop_output", "output_variables")
rm(list=setdiff(ls(),c(network_variables,base_variables,simulation_variables,output_variables)))


