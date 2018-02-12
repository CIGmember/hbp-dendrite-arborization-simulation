##There seem to be some mistakes probably having to do with the models placing the predictions in incorrect fields in the dataframes,
##i have to check that out, but at least now i know things are correct structurally.


simulate_neuron_with_tortuosity <- function(seed=1)
{


  max_id_node=0; #Maximo id de los nodos que pertenecen a la neurona. Necesario para generar los nuevos ids de los nodos.
  desc_model<-neurostr::desc_model
  simulation_model<-neurostr::simulation_model
  neo_tortosity_model <- neurostr::neo_tortosity_model
  model_first_bif <- neurostr::model_first_bif


  Prototype_Neuron<-neurostr::Prototype_Neuron
  neuron <- get_simulated_neuron(Prototype_Neuron)
  id_cut_nodes <- neuron[[3]]
  neuron_2 <- neuron
  id_cut_nodes_2 <- id_cut_nodes


  t<-0
  while(length(id_cut_nodes)>0)
  {
    features<-rbindlist(node_feature_extractor(neuron$plain,id_nodes=id_cut_nodes))

    selected_features<-features[,colnames(features)%in%names(desc_model$params),with=F]
    selected_features$node_order[selected_features$node_order>4]<-4
    selected_features$node_order<-factor(selected_features$node_order,levels=0:4)


    desc_probabilities<-pred_BN(desc_model,selected_features)

    #Sampled number of descendant radonmly according to the probability of terminal, continue o bifurcation for each node
    num_of_descendant<-apply(desc_probabilities[,grep("prob",colnames(desc_probabilities))],1,function(x){sample(c(0,1,2),size=1,prob=x)})

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



        if(nrow(simulation_data[num_of_descendant==1])>0)
        {
          simulated_data<-rbind(simulation_continue,simulation_bifurcation)
        }else{
          simulated_data<-simulation_bifurcation
        }
      }

      simulated_data$desc_length<-exp(simulated_data$desc_length)

      if(t > 0){
        simulated_tortosity <- compute_ready_state_for_neotortosity(neo_tortosity_model,simulation_continue,simulation_bifurcation,simulation_data,num_of_descendant)

        if(t>1){
          for(k in 1:length(correspondence[[1]])){
            el_cor <- correspondence[k,]
            prev_simulated_data$id_parent[prev_simulated_data$id_parent == el_cor[[1]]]<-el_cor[[2]]
          }
        }
        max_id_node_2 <- max(id_cut_nodes_2)
        neuron_2 <- tortosity_integration_neo(neuron_2,prev_simulation_data,simulated_tortosity,prev_simulated_data,prev_num_of_descendant,id_cut_nodes_2,max_id_node_2)#max_id_node) idk if changing this is right
        id_cut_nodes_2 = neuron_2$ids

        correspondence<- matrix(1:(length(id_cut_nodes)*2),ncol = 2)
        correspondence <- as.data.frame(correspondence)
        names(correspondence)<- c("ids_eps60","ids_eps0")
        correspondence$ids_eps60<-id_cut_nodes
        correspondence$ids_eps0<-id_cut_nodes_2



        write_2_JSON(neuron_2,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/newneuroneps02.json")

      }
      prev_simulation_data <- simulation_data
      prev_simulated_data <- simulated_data
      prev_num_of_descendant <-num_of_descendant



      is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1



      max_id_node=max(id_cut_nodes)
      neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)

      write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/newneuroneps602.json")

      id_cut_nodes<-neuron$ids
      print(id_cut_nodes)
      max_id_node=max(id_cut_nodes)




    }


    else{
      id_cut_nodes=c()
    }


    t<-t+1
  }

  return(neuron)

}


compute_ready_state_for_neotortosity <- function(neo_tortosity_model,simulation_continue,simulation_bifurcation,simulation_data,num_of_descendant){

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
  #neuron$ids <- final_ids[final_ids_positions]
  neuron$ids <- final_ids[order(final_ids_positions,final_ids)] #FINALLY FOUND THE MISTAKE!!!!!!


  return(neuron)




}

