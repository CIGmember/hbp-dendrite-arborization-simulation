simulate_neuron_5 <- function(seed=1)
{
  #/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments

  # seed=1

  #load("~/Desktop/Simulacion-arborizaciones-basales/R-project/Pablo's neurostr/GEnv.RData")

  max_id_node=0; #Maximo id de los nodos que pertenecen a la neurona. Necesario para generar los nuevos ids de los nodos.

  # set.seed(seed)
  #Aprender o precargar modelos para clasificar y simular nodos terminales
  desc_model<-neurostr::desc_model
  simulation_model<-neurostr::simulation_model
  #tortosity_model<-load(file ="./data/tortosity.rda")
  neo_tortosity_model <- neurostr::neo_tortosity_model
  #load("./data/tortosity_model.rda")#model_tortosity
 # load("./data/retrained.rda")
 # load("./data/neo_desc_model.rda")
 # load("./data/branch_length_nodes.rda")
 # load("./data/model_first_bif.rda")
 model_first_bif <- neurostr::model_first_bif
  #Leer neurona indicada por el usuario
  # file_path<-"/home/universidad/Documents/neuron_data/datos/All/h213III1.DAT"
  #file_path<-"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/HUMANOS_IIIyV/III2-3cmDAT/h213III1.DAT"
  #  file_path<-"/home/universidad/datos/Cortadas/human cing id if6 porta 2 sec1 cel20.DAT"

  #neuron<-neuro_converter(file_path,eps=60)

  #Simular los nodos raíz de las neuritas basales de una neurona

  #id_cut_nodes<-get_cut_nodes(neuron$plain)

  neuron <- get_simulated_neuron()
  id_cut_nodes <- neuron[[3]]

  neuron_2 <- neuron
  id_cut_nodes_2 <- id_cut_nodes





  ###Repetir hasta que la lista de nodos a crecer se encuentre vacía###
  #####################################################################
  #Aprender features para los nodos cortados
  t<-0
  while(length(id_cut_nodes)>0)
  {
    features<-rbindlist(node_feature_extractor(neuron$plain,id_nodes=id_cut_nodes))

    selected_features<-features[,colnames(features)%in%names(desc_model$params),with=F]
    selected_features$node_order[selected_features$node_order>4]<-4
    selected_features$node_order<-factor(selected_features$node_order,levels=0:4)


    #Clasificar nodos cortados en terminal, continuar o bifurcar
    desc_probabilities<-pred_BN(desc_model,selected_features)

    #selected_branch_features <- branch_feature_extractor(neuron$plain)
    #selected_branch_features <- selected_branch_features$measures
    #selected_branch_features<-selected_branch_features[selected_branch_features$centrifugal_order == (t+1),]

    #branch_desc_probabilities <- pred_BN(branch_desc_model,selected_branch_features[,c(3,5,7,8,9,10)])

    #Sampled number of descendant radonmly according to the probability of terminal, continue o bifurcation for each node
    num_of_descendant<-apply(desc_probabilities[,grep("prob",colnames(desc_probabilities))],1,function(x){sample(c(0,1,2),size=1,prob=x)})
    #num_of_descendant<-apply(branch_desc_probabilities[,grep("prob",colnames(branch_desc_probabilities))],1,function(x){sample(c(0,1,2),size=1,prob=x)})


    if(sum(num_of_descendant)> 42){#hard limit to avoid crazy growth in the model
      num_of_descendant = num_of_descendant*0+1
    }


    if(sum(num_of_descendant)>0)
    {
      #Simular nuevos nodos
      simulation_data<-features[,colnames(features)%in%names(simulation_model$continue$structure$nodes),with=F]
      simulation_data$node_order[simulation_data$node_order>4]<-4
      simulation_data$node_order<-factor(simulation_data$node_order,levels=0:4)

      if(nrow(simulation_data[num_of_descendant==1])>0)
      {
        continue_data<-simulation_data[num_of_descendant==1]
        simulation_continue<-simulate_continuation(simulation_model$continue,continue_data)
        simulation_continue$id_parent<-id_cut_nodes[num_of_descendant==1]
        simulated_data<-simulation_continue


        #simulation_continue$desc_length <- exp(simulation_continue$desc_length)

        #FINISH EMPTY TORTOSITY, CHANGE THE SIMULATION VALUES FOR THE N NODES GOOD VALUES, REMEBER THE CENTRIFUGAL ORDER IS T+2 AND HAS TO BE INCLUDED IN A PROTOTYPE DATAFRAME FOR TORTOSITY INTEGRATION TO PROCESS IT,
        #THEN CHECK THE BRANCH MODEL THAT WE IMPLEMENTED AND SEE HOW IT WORKS ALL TOGETHER AND HOW MUCH IT IMPROVES THE SIMULATION, THEN CONTINUE WITH THE FIXES AND SO ON, FOCUSNESS IS AWESOME AND I LOVE IT AND MYSELF IN IT

      #  simulated_tortosity<-simulate_neo_tortosity(neo_tortosity_model[[1]],simulation_continue,continue_data,num_of_descendant,1)
       # simulated_tortosity<-simulate_tortosity(model_tortosity,simulation_continue,continue_data)
       # simulated_tortosity<-simulate_empty_tortosity(branch_length_nodes_model,simulation_continue,t+2)

      #  simulated_tortosity_2[[1]] <- simulated_tortosity[[1]]
      #  simulated_tortosity <- simulated_tortosity_2

      #  simulation_continue <- simulated_data


        if(!identical(names(simulation_continue),c("desc_azimuth_angle","desc_elevation_angle","desc_length","id_parent"))){
          simulation_continue<- simulation_continue[,order(names(simulation_continue),c("desc_azimuth_angle","desc_elevation_angle","desc_length","id_parent"))]
        }





      }
      if(nrow(simulation_data[num_of_descendant==2])>0)
      {
        bifurcation_data<-simulation_data[num_of_descendant==2]
        simulation_bifurcation<-simulate_bifurcation(simulation_model$bifurcation,bifurcation_data)
        second_branch_idx<-grep("2",colnames(simulation_bifurcation))
        simulation_bifurcation_2<-simulation_bifurcation[,second_branch_idx]
        colnames(simulation_bifurcation_2)<-gsub("2","",colnames(simulation_bifurcation_2))
        simulation_bifurcation<-rbind(simulation_bifurcation[,setdiff(1:ncol(simulation_bifurcation),second_branch_idx)],simulation_bifurcation_2)
        simulation_bifurcation$id_parent<-rep(id_cut_nodes[num_of_descendant==2],2)



       #simulation_bifurcation$desc_length<-exp(simulation_bifurcation$desc_length)


      #  simulated_tortosity<-simulate_neo_tortosity(neo_tortosity_model[[2]],simulation_bifurcation,bifurcation_data,num_of_descendant,2)
       # simulated_bifurcation_tortosity <- simulate_tortosity(model_tortosity,simulation_bifurcation,bifurcation_data)
       #simulated_bifurcation_tortosity<-simulate_empty_tortosity(branch_length_nodes_model,simulation_bifurcation,t+2)

       # simulated_bifurcation_tortosity_2[[1]] <- simulated_bifurcation_tortosity[[1]]
       # simulated_bifurcation_tortosity <- simulated_bifurcation_tortosity_2


        if(nrow(simulation_data[num_of_descendant==1])>0)
        {
          simulated_data<-rbind(simulation_continue,simulation_bifurcation)
      #    simulated_tortosity <- append(simulated_tortosity, simulated_bifurcation_tortosity)
        }else{
          simulated_data<-simulation_bifurcation
      #    simulated_tortosity <- simulated_bifurcation_tortosity
        }
      }

      simulated_data$desc_length<-exp(simulated_data$desc_length)
      #we probably need to recalculate this.

      #simulation bifurcation is from last iteration, needs to be cleaned for the new one, this may solve the issue if not we have to go inside the method to try to verify whats the exact
      #rows that need to be processed
      #On another topic, we have to solve the problem with the tortosity looking like a zig zag, maybe the distances problem is solved now but still, we need it to be right



      if(t > 0){
        simulated_tortosity <- compute_ready_state_for_neotortosity(neo_tortosity_model,simulation_continue,simulation_bifurcation,simulation_data,num_of_descendant)

        #here we create simulated data with cont and bif and make sure there are no semantic mistakes

       # id_cut_nodes_2 = neuron_2$id_cut_nodes

        #here i have to make a correspondence between id_cut_nodes and id_cut_nodes_2, replacing the parents in the eps60 model for the parents in the eps0 model
        if(t>1){
          for(k in 1:length(correspondence[[1]])){
            el_cor <- correspondence[k,]
            prev_simulated_data$id_parent[prev_simulated_data$id_parent == el_cor[[1]]]<-el_cor[[2]]
          }
          #prev_simulation_data$id_parent <- correspondence$eps0

        }


        max_id_node_2 <- max(id_cut_nodes_2)
        neuron_2 <- tortosity_integration_neo(neuron_2,prev_simulation_data,simulated_tortosity,prev_simulated_data,prev_num_of_descendant,id_cut_nodes_2,max_id_node)
        id_cut_nodes_2 = neuron_2$ids

        #idk if the order fucks it up, because we get the continues up, not in their spot,w ehave to see if coherency is maintained

        correspondence<- matrix(1:(length(id_cut_nodes)*2),ncol = 2)
        correspondence <- as.data.frame(correspondence)
        names(correspondence)<- c("ids_eps60","ids_eps0")
        correspondence$ids_eps60<-id_cut_nodes
        correspondence$ids_eps0<-id_cut_nodes_2
        #additional needs
        write_2_JSON(neuron_2,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/newneuroneps0.json")

      }
      prev_simulation_data <- simulation_data
      prev_simulated_data <- simulated_data
      prev_num_of_descendant <-num_of_descendant



      is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
      #Añadir nuevos nodos a la neurona


      max_id_node=max(id_cut_nodes)
      neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node

      write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/newneuroneps60.json")

       id_cut_nodes<-neuron$ids
      print(id_cut_nodes)
      max_id_node=max(id_cut_nodes)




    }


    else{
      id_cut_nodes=c()
    }
#here last step?






    t<-t+1
  }

 # write_2_JSON(neuron,filename)
  #Fin repetir


  #neuron <- tortosify_neuron(neuron) DISABLED UNTIL TORTOSITY IMPLEMENTATION PHASE

  return(neuron)

}


compute_ready_state_for_neotortosity <- function(neo_tortosity_model,simulation_continue,simulation_bifurcation,simulation_data,num_of_descendant){
  #now we have what we should have had, the desc field of the branches to insert, i need to make sure of the order to continue

#we need to create false data for terminal nodes

  descendants_b <-num_of_descendant==2
  descendants_c <-num_of_descendant!=2
  descendants_0 <- num_of_descendant==0



  if(sum(descendants_b)>0){
  bifs_tort <- simulate_neo_tortosity(neo_tortosity_model$bifurcation,simulation_bifurcation,simulation_data[descendants_b],num_of_descendant,2)
  lbt <- length(bifs_tort)
  }
  else{
    bifs_tort <- list()
  }

  sd0 <- sum(descendants_0)

  if(sd0>0){
    simulation_continue_1 <- matrix(0,nrow = (sum(descendants_c)),ncol = 4)
    simulation_continue_1 <- as.data.frame(simulation_continue_1)

    if(sum(num_of_descendant==1)==0){
      setnames(simulation_continue_1,c("desc_azimuth_angle","desc_elevation_angle","desc_length","id_parent" ))
      simulation_continue <- simulation_continue_1
    }
    else{
      setnames(simulation_continue_1,names(simulation_continue))
      descendants_rest <- num_of_descendant[num_of_descendant!=2]!=0
      simulation_continue_1[descendants_rest,]<- simulation_continue
      #simulation_continue_1[descendants_0,]<- simulation_continue_0
      simulation_continue <- simulation_continue_1
    }
  }

if(sum(descendants_c)>0){
  cont_tort <- simulate_neo_tortosity(neo_tortosity_model$continue,simulation_continue,simulation_data[descendants_c],num_of_descendant,1)
  lct <- length(cont_tort)
}
  else{
    cont_tort <- list()
  }



  simulation_tortosity <- c(cont_tort,bifs_tort)#maybe this fixes something, although i doubt is the real solution.
  simulation_tortosity[descendants_b]<-bifs_tort
  simulation_tortosity[descendants_c]<-cont_tort




  #send to simulate the ones with 2s and the ones with 1s as 2, and the ones with 0s as 1. Fuck, the ones that say 1, they need to go in the other one...


return(simulation_tortosity)


}


tortosity_integration_neo <- function(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node){

  initial_ids <- id_cut_nodes
  #This may be corrected in the future, since we are overcomputing the cases with 0.
  filtering <- which(num_of_descendant==0)
  if(length(filtering)>0){
    initial_ids <- initial_ids[-filtering]
  }


  #neuron$plain
  is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
  #neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
  #id_cut_nodes<-neuron$ids
  final_ids <-c()

  final_ids_positions <-order(sapply(simulated_tortosity, function (x)  length(x[[1]])))

  not_done = 1
  cont=1
  while(not_done){

    #so the point here is to intrude card_ord with newly created data that comes from the retrained model. The only problem is that i need to address the case of num
    #descendants=2, i have to see it in the code. En principio, el numero de ids y el datasetde tortosity integration already give me a perfect pattern for tortosity
    #integration, i just have to "plug_in" the new values, rendering many of the previous computation useless, but if it works we would be satisfied.


    #see what i need to fix here
    card_ord <- lapply(simulated_tortosity, function (x) x[cont,])
    card_ord <- rbindlist(card_ord)
    #card_ord <- next_wide_output()
    #is just neuron extract wirth the new ids

    if(cont >1){
      is_bifurcation <- c(rep(0,length(card_ord$desc_azimuth_angle)))
      card_ord$id_parent <- id_cut_nodes#we still dont know what will happen later, and 100% if this is correct
      outs <- which(is.na(card_ord$desc_azimuth_angle))

      #card_ord$desc_azimuth_angle <- simulation_continue$desc_azimuth
      #card_ord$desc_elevation_angle <- simulation_continue$desc_elevation
      #card_ord[,3] <- exp(simulation_continue[,3])

      if(length(outs)>0){
        final_ids <- append(final_ids,id_cut_nodes[outs])
        card_ord <- card_ord[-outs,]
        is_bifurcation <- is_bifurcation[-outs]
        simulated_tortosity <- simulated_tortosity[-outs]
      }
      if(length(simulated_tortosity) == 0){
        not_done <- 0
      }

    }
    else{
      card_ord$id_parent <- simulated_data$id_parent#risky but lets see
      #first node incorrect, lets see what we do
    }
    max_id_node=max(id_cut_nodes)

    #write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ongrowingneuron.json")

    neuron<-simulated_node_coordinates(neuron$plain, data.matrix(card_ord), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
    id_cut_nodes<-neuron$ids
    max_id_node=max(id_cut_nodes)
    cont <- cont + 1

  }
  neuron$ids <- final_ids[final_ids_positions]



  return(neuron)




}



tortosify_neuron <- function(neuron){

  neur = 1
  neurites <- neuron$data$neuron$neurites[[1]]
  initial_nodes <- rbindlist(neurites$tree$nodes)

  new_neuron <- neuron
  new_neuron$data$neurons$neurites[[1]]$tree$children <- NULL
  new_neuron$ids <- NULL
  json_neur <- toJSON(new_neuron$data)
  new_neuron$plain <-json_neur

  #now we have a blank canvas to work with.



  for(i in 1:length(neurites)){
    neurite <- neurites[i,]$tree
    neurite_empezada <- neurite$children[[1]]
    #here we integrate the previous case into the neuron
    neurite_tortosified <- tortosify_neurite(neurite_empezada,new_neuron)
    neurites[i,]$tree <- neurite_tortosified


  }
  #neuron$data$neuron$neurites <- neurites

  return(neuron)
}


tortosify_neurite <- function(neurite,new_neuron){

  #data_child <- data.frame(length.sigma = 0,length.mean=0,n_nodes=0,total_length=0,angle.sigma=0, angle.mean=0,comp_length.sigma=0, comp_length.mean=0)
  branch_beginnings <- neurite$root

  for(i in 1:length(branch_beginnings[,1])){

    branch_beginning <- branch_beginnings[i,]#for some reason duplicates are included

    #nodes <- rbindlist(brancher$nodes)
    nodes <- neurite$nodes[[i]]
    #it should be only one, so
    nodes <- nodes[1,]
    #the rest are undesired tortosity

    num_nodes <-length(nodes$id)
    branch_end <-nodes[length(nodes$id),]

    #here i have to include the simulation of tortosity

    #here i have to include the inclusion of the branch in the neurite, and the visualization oportunity


    if("children"%in% names(neurite)){

      for(i in length(neurite$children)){


        #here i have to define the recursive exploration

        #data_child <-rbind(data_child,extract_branch_data(neurite$children[[i]]))


        #tortosify_neurite(neurite$children[[i]])
      }
    }







  }
}

tortosity_integration_neo <- function(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node){



  #this method needs to work for when its called in the middle of adding datapoints.

  initial_ids <- id_cut_nodes

  #This may be corrected in the future, since we are overcomputing the cases with 0.
  filtering <- which(num_of_descendant==0)

  if(length(filtering)>0){
    initial_ids <- initial_ids[-filtering]
  }


  #neuron$plain
  is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
  #neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
  #id_cut_nodes<-neuron$ids
  final_ids <-c()


  not_done = 1
  cont=1
  while(not_done){

    card_ord <- lapply(simulated_tortosity, function (x) x[cont,])
    card_ord <- rbindlist(card_ord)



    if(cont >1){
      is_bifurcation <- c(rep(0,length(card_ord$desc_azimuth_angle)))
      card_ord$id_parent <- id_cut_nodes#we still dont know what will happen later, and 100% if this is correct
      outs <- which(is.na(card_ord$desc_azimuth_angle))

      if(length(outs)>0){
        final_ids <- append(final_ids,id_cut_nodes[outs])
        card_ord <- card_ord[-outs,]
        is_bifurcation <- is_bifurcation[-outs]
        simulated_tortosity <- simulated_tortosity[-outs]
      }
      if(length(simulated_tortosity) == 0){
        not_done <- 0
      }


    }

    max_id_node=max(id_cut_nodes)

    write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ongrowingneuron.json")

    neuron<-simulated_node_coordinates(neuron$plain, data.matrix(card_ord), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
    id_cut_nodes<-neuron$ids
    max_id_node=max(id_cut_nodes)
    cont <- cont + 1

  }
  neuron$ids <- final_ids

  return(neuron)




}







#here we repeat in a loop the integration of the different calculated dots for the tortosity model
#we have the original data from sergio and the expanded simulated tortosity data, conveniently
#separated into 2 variables. We know that the tortosity data uses the id of the sergio's simulated node as first id of our first tortosity simulated node,
#but the values of azimuth and elevation calculated for sergio are maintained for the first datapoint, while length is simulated, the rest of the points are built on top, but now
#we know we needed the output of simulated_node_coordinates to address them. Since we know that all simulated datapoints per branch are in a chain parent->child, we can then rewrite the
#ids at convenience so we can add the following ones through repeated alls to simulate_node_coordinates until all tortosity datapoints are included and the neuron is ready to receive more.
#is bifurcation will only be bifurcation for the datapoints that sergio calculated, the rest are 0s since they simply continue the
tortosity_integration_or <- function(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node){



  #this method needs to work for when its called in the middle of adding datapoints.

  initial_ids <- id_cut_nodes

  #This may be corrected in the future, since we are overcomputing the cases with 0.
  filtering <- which(num_of_descendant==0)

  if(length(filtering)>0){
    initial_ids <- initial_ids[-filtering]
  }


  #neuron$plain
  is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
  #neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
  #id_cut_nodes<-neuron$ids
  final_ids <-c()


  not_done = 1
  cont=1
  while(not_done){

    card_ord <- lapply(simulated_tortosity, function (x) x[cont,])
    card_ord <- rbindlist(card_ord)



    if(cont >1){
      is_bifurcation <- c(rep(0,length(card_ord$desc_azimuth_angle)))
      card_ord$id_parent <- id_cut_nodes#we still dont know what will happen later, and 100% if this is correct
      outs <- which(is.na(card_ord$desc_azimuth_angle))

      if(length(outs)>0){
        final_ids <- append(final_ids,id_cut_nodes[outs])
        card_ord <- card_ord[-outs,]
        is_bifurcation <- is_bifurcation[-outs]
        simulated_tortosity <- simulated_tortosity[-outs]
      }
      if(length(simulated_tortosity) == 0){
        not_done <- 0
      }


    }

    max_id_node=max(id_cut_nodes)

    write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ongrowingneuron.json")

    neuron<-simulated_node_coordinates(neuron$plain, data.matrix(card_ord), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
    id_cut_nodes<-neuron$ids
    max_id_node=max(id_cut_nodes)
    cont <- cont + 1

  }
  neuron$ids <- final_ids

  return(neuron)




}


tortosity_integration_2 <- function(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node){


  #this method needs to work for when its called in the middle of adding datapoints.

  initial_ids <- id_cut_nodes

  #This may be corrected in the future, since we are overcomputing the cases with 0.
  filtering <- which(num_of_descendant==0)

  if(length(filtering)>0){
    initial_ids <- initial_ids[-filtering]
  }


  #neuron$plain
  is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
  #neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
  #id_cut_nodes<-neuron$ids
  final_ids <-c()


  not_done = 1
  cont=1
  while(not_done){

    #so the point here is to intrude card_ord with newly created data that comes from the retrained model. The only problem is that i need to address the case of num
    #descendants=2, i have to see it in the code. En principio, el numero de ids y el datasetde tortosity integration already give me a perfect pattern for tortosity
    #integration, i just have to "plug_in" the new values, rendering many of the previous computation useless, but if it works we would be satisfied.


    #see what i need to fix here
    card_ord <- lapply(simulated_tortosity, function (x) x[cont,])
    card_ord <- rbindlist(card_ord)

    #card_ord <- next_wide_output()



    #is just neuron extract wirth the new ids




    if(cont >1){
      is_bifurcation <- c(rep(0,length(card_ord$desc_azimuth_angle)))
      card_ord$id_parent <- id_cut_nodes#we still dont know what will happen later, and 100% if this is correct
      outs <- which(is.na(card_ord$desc_azimuth_angle))


      #new evidence
      features<-rbindlist(node_feature_extractor(neuron$plain,id_nodes=id_cut_nodes))
      simulation_data<-features[,colnames(features)%in%names(simulation_model$continue$structure$nodes),with=F]
      simulation_data$node_order[simulation_data$node_order>4]<-4
      simulation_data$node_order<-factor(simulation_data$node_order,levels=0:4)

      # if(nrow(simulation_data[num_of_descendant==1])>0)
      #{
      #  continue_data<-simulation_data[num_of_descendant==1]


      #continue_data<-simulation_data[num_of_descendant==1]
      simulation_continue<-simulate_continuation(sergio_retrained$continue,simulation_data)

      card_ord$desc_azimuth_angle <- simulation_continue$desc_azimuth
      card_ord$desc_elevation_angle <- simulation_continue$desc_elevation
      card_ord[,3] <- exp(simulation_continue[,3])



      if(length(outs)>0){
        final_ids <- append(final_ids,id_cut_nodes[outs])
        card_ord <- card_ord[-outs,]
        is_bifurcation <- is_bifurcation[-outs]
        simulated_tortosity <- simulated_tortosity[-outs]
      }

      if(length(simulated_tortosity) == 0){
        not_done <- 0
      }





    }

    max_id_node=max(id_cut_nodes)

    write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ongrowingneuron.json")

    neuron<-simulated_node_coordinates(neuron$plain, data.matrix(card_ord), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
    id_cut_nodes<-neuron$ids
    max_id_node=max(id_cut_nodes)
    cont <- cont + 1

  }
  neuron$ids <- final_ids



  return(neuron)




}


