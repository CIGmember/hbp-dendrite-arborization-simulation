


#' This function calls the simulator of a neuron
#'
#' This function calls thw simulator of a neuron
#' @return The nodes of the neuron as a list
#' @example
#' neuron_nodes <- get_simulated_root_nodes()
get_simulated_root_nodes <- function(){
  neuron <-get_simulated_neuron()
  return(neuron$nodes)
}


#' This function simulates a neuron.
#'
#'This function simulates a neuron. More specifically, it simulates the soma as a spherical radius, the number of dendrites and
#'their placement in the soma as root nodes, and the first branch order of each dendrite.
#'
#' @return a neuron object similar to those return by the neuro_converter function in neuroSTR
#' @example
#' neuron <- get_simulated_neuron()
get_simulated_neuron <- function(){
  neuron <-1
  #λ = 131, 51 y ν = 2, 93 for the ConwatMaxwellpoisson distribution
  N_root_nodes <- 1
  while(N_root_nodes < 3 | N_root_nodes >5 ){
       N_root_nodes <- rcom(1, 131.51, 2.93, log.z = NULL)
       }

  root_nodes_angles <- vonMisesMultiSim(N_root_nodes)
  soma_radius <- rlnorm(1,meanlog = 2.15,sdlog = 0.299)


  neur1_root <- c(soma_radius,0)
  root_nodes_position <- list()
  root_nodes_position[[1]]<- (c(neur1_root,0))
  current_neur= neur1_root
  for(i in 1:length(root_nodes_angles)){
    current_neur <-Rotation(matrix(current_neur,nrow=1,ncol=2),as.numeric(root_nodes_angles[i]))
    root_nodes_position[[i+1]]<- (c(current_neur,0))
  }

  sim_neur <- create_simulated_neuron(soma_radius,N_root_nodes,root_nodes_position)
  json_neur <- toJSON(sim_neur)
  id_root_nodes <- ((2*N_root_nodes+1): (3*N_root_nodes))
  neuron <- list(plain = json_neur,data = sim_neur,id_root_nodes = id_root_nodes)
  return(neuron)
}


#' This function creates a neuron object with the parameters specified inside get_simulated_neuron.
#'
#' This function creates a neuron object with the parameters specified inside get_simulated_neuron.
#'
#' @param soma_radius is a real valued quantity indicating the radius of the simulated soma
#' @param N_root_nodes is an integer value that indicates the number of dendrites, currently in the interval [3,5].
#' @param root_nodes_position is a list containing the coordinates of the root nodes of each dendrite in the soma
#'
#' @return a neuron object similar to those return by the neuro_converter function in neuroSTR
create_simulated_neuron<- function(soma_radius,N_root_nodes,root_nodes_position){

  model_first_bif <- neurostr::model_first_bif
  simulated_neuron <- Prototype_Neuron #Variable loaded in global environment
  simulated_neuron$neurons$id<-"Simulated.DAT_1"
  simulated_neuron$neurons$soma$nodes[[1]]$id = 0
  simulated_neuron$neurons$soma$nodes[[1]]$x = 0
  simulated_neuron$neurons$soma$nodes[[1]]$y = 0
  simulated_neuron$neurons$soma$nodes[[1]]$z = 0
  simulated_neuron$neurons$soma$nodes[[1]]$r = soma_radius

  n_index = 1
  simulated_neuron$neurons$neurites[[1]] <- simulated_neuron$neurons$neurites[[1]][1:N_root_nodes,]
  nodes_id_count=N_root_nodes+1

  for(i in 1:N_root_nodes){
    simulated_neuron$neurons$neurites[[1]][i,]
    simulated_neuron$neurons$neurites[[1]][i,]$id = i
    simulated_neuron$neurons$neurites[[1]][i,]$type = 3

    simulated_neuron$neurons$neurites[[1]][i,]$tree$nodes[[1]]$id = nodes_id_count

    simulated_neuron$neurons$neurites[[1]][i,]$tree$nodes[[1]]$x = root_nodes_position[[i]][1]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$nodes[[1]]$y = root_nodes_position[[i]][2]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$nodes[[1]]$z = root_nodes_position[[i]][3]
    nodes_id_count = nodes_id_count +1


    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$root$id = nodes_id_count
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$root$x = root_nodes_position[[i]][1]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$root$y = root_nodes_position[[i]][2]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$root$z = root_nodes_position[[i]][3]

    dot <- simulate_first_bifurcation(model_first_bif,list(x2.x = root_nodes_position[[i]][1],y2.y = root_nodes_position[[i]][2],z2.z = root_nodes_position[[i]][3]))

    #Add simulated first dots.
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$nodes[[1]]$id = 2*N_root_nodes+i
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$nodes[[1]]$x = dot[[1]][1]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$nodes[[1]]$y = dot[[1]][2]
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$nodes[[1]]$z = dot[[1]][3]

    #eliminate the rest of the original tree
    if("children" %in% names(simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]])){
    simulated_neuron$neurons$neurites[[1]][i,]$tree$children[[1]]$children <- NULL
    }
  }
return(simulated_neuron)

}


#' This function samples from a multivariate von mises distribution.
#'
#' This function samples from a multivariate von mises distribution.
#' @param N_root_nodes is an integer value that indicates the number of dendrites, currently in the interval [3,5].
#'
#' @return a vector of N_root_nodes angular coordinates in a circle.
vonMisesMultiSim <- function(N_root_nodes){
  if(N_root_nodes == 3)
  {
    samples <- rmvVonMises(1, c(1.28,2.45), c(3.969,4.992), matrix(c(0,6.5e-12,6.5e-12,0),ncol=2,nrow=2) )
  }
  if(N_root_nodes == 4)
  {
    samples <- rmvVonMises(1, c(1.832,1.176,1.778), c(2.689,2.390,2.460), matrix(c(0,-0.00204,6.36e-05,-0.00204,0,-0.710,6.36e-05,-0.710,0),ncol=3,nrow=3) )
  }
  if(N_root_nodes == 5)
  {
    samples <- rmvVonMises(1, c(1.316,1.243,1.310,0.970), c(2.577,2.413,2.387,2.266), matrix(c(0,-0.000198,-0.000187,0.0235,-0.000198,0,-0.00118,0.000305,-0.000187,-0.00118,0,-0.681,0.0235,0.000305,-0.681,0),ncol=4,nrow=4) )
  }

  return(samples)

}
