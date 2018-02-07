#simulate_neuron<-function(seed=1)

library(FNN)
library(entropy)
library(MASS)
library(rbmn)
library(pracma)
library(compoisson)
library(devtools)
library(mvCircular)
library(stats)
library(spdep)
library(jsonlite)
library(data.table)
library(FastKNN)

simulate_neuron_2 <- function()
{


max_id_node=0; #Maximo id de los nodos que pertenecen a la neurona. Necesario para generar los nuevos ids de los nodos.

#set.seed(seed)
#models_retrained is a global env variable modified in the shinny interface code

if(models_retrained==0){
  desc_model<-neurostr::desc_model
  simulation_model<-neurostr::simulation_model
  model_first_bif <- neurostr::model_first_bif
}
  else{
  load(file="./desc_model_retrained.rda")
  load(file="./simulation_model_retrained.rda")
  load(file="./model_first_bif_retrained.rda")
  desc_model <- desc_model_retrained
  simulation_model <- simulation_model_retrained
  model_first_bif<- model_first_bif_retrained
  }




  #id_cut_nodes<-get_cut_nodes(neuron$plain)
  neuron <- get_simulated_neuron()
  id_cut_nodes <- neuron[[3]]


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
      is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1

      #Añadir nuevos nodos a la neurona
      neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
      id_cut_nodes<-neuron$ids
      print(id_cut_nodes)
      max_id_node=max(id_cut_nodes)

    }else{
      id_cut_nodes=c()
    }
    t<-t+1
  }

  return(neuron)

}
