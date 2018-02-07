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

simulate_neuron_4_mod <- function(seed=1)
{
  #/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments

  # seed=1

  #load("~/Desktop/Simulacion-arborizaciones-basales/R-project/Pablo's neurostr/GEnv.RData")

  max_id_node=0; #Maximo id de los nodos que pertenecen a la neurona. Necesario para generar los nuevos ids de los nodos.


  train_models = 0

  if(train_models){
    datamuld<- get_features_files("/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/HUMANOS_IIIyV/TodosDAT",eps=60)
    do2 <- datamuld[datamuld$node_order == 2,]
    do3 <- datamuld[datamuld$node_order == 3,]
    do4 <- datamuld[datamuld$node_order == 4,]

    simulation_model_2 <-compute_simulation_models(do2,num_restart = 5,max_iter = Inf)
    simulation_model_3 <-compute_simulation_models(do3,num_restart = 5,max_iter = Inf)
    simulation_model_4 <-compute_simulation_models(do4,num_restart = 5,max_iter = Inf)

    save(simulation_model_2,file = "./data/simulation_model_2.rda")
    save(simulation_model_3,file = "./data/simulation_model_3.rda")
    save(simulation_model_4,file = "./data/simulation_model_4.rda")




  }


  # set.seed(seed)
  #Aprender o precargar modelos para clasificar y simular nodos terminales
  desc_model<-neurostr::desc_model
  simulation_model<-neurostr::simulation_model
  simulation_model_2<-neurostr::simulation_model_2
  simulation_model_3<-neurostr::simulation_model_3
  simulation_model_4<-neurostr::simulation_model_4
  #tortosity_model<-load(file ="./data/tortosity.rda")
  tortosity_model <- neurostr::model_tortosity
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


  ###Repetir hasta que la lista de nodos a crecer se encuentre vacía###
  #####################################################################
  #Aprender features para los nodos cortados
  t<-2
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

    if(sum(num_of_descendant)>0)
    {
      #Simular nuevos nodos
      simulation_data<-features[,colnames(features)%in%names(simulation_model$continue$structure$nodes),with=F]
      simulation_data$node_order[simulation_data$node_order>4]<-4
      simulation_data$node_order<-factor(simulation_data$node_order,levels=0:4)

      if(nrow(simulation_data[num_of_descendant==1])>0)
      {
        continue_data<-simulation_data[num_of_descendant==1]


        if(t==2){
          simulation_continue<-simulate_continuation(simulation_model_2$continue,continue_data)
        }
        else if(t==3)
        {
          simulation_continue<-simulate_continuation(simulation_model_3$continue,continue_data)
        }
        else{
          simulation_continue<-simulate_continuation(simulation_model_4$continue,continue_data)
        }


        simulation_continue$id_parent<-id_cut_nodes[num_of_descendant==1]
        simulated_data<-simulation_continue


        #simulation_continue$desc_length <- exp(simulation_continue$desc_length)

        #FINISH EMPTY TORTOSITY, CHANGE THE SIMULATION VALUES FOR THE N NODES GOOD VALUES, REMEBER THE CENTRIFUGAL ORDER IS T+2 AND HAS TO BE INCLUDED IN A PROTOTYPE DATAFRAME FOR TORTOSITY INTEGRATION TO PROCESS IT,
        #THEN CHECK THE BRANCH MODEL THAT WE IMPLEMENTED AND SEE HOW IT WORKS ALL TOGETHER AND HOW MUCH IT IMPROVES THE SIMULATION, THEN CONTINUE WITH THE FIXES AND SO ON, FOCUSNESS IS AWESOME AND I LOVE IT AND MYSELF IN IT

       # simulated_tortosity<-simulate_tortosity(model_tortosity,simulation_continue,continue_data)
       # simulated_tortosity<-simulate_empty_tortosity(branch_length_nodes_model,simulation_continue,t+2)

      #  simulated_tortosity_2[[1]] <- simulated_tortosity[[1]]
      #  simulated_tortosity <- simulated_tortosity_2

        #simulation_continue <- simulated_data

      }
      if(nrow(simulation_data[num_of_descendant==2])>0)
      {
        bifurcation_data<-simulation_data[num_of_descendant==2]


        if(t==2){
          simulation_bifurcation<-simulate_bifurcation(simulation_model_2$bifurcation,bifurcation_data)
        }
        else if(t==3)
        {
          simulation_bifurcation<-simulate_bifurcation(simulation_model_3$bifurcation,bifurcation_data)
        }
        else{
          simulation_bifurcation<-simulate_bifurcation(simulation_model_4$bifurcation,bifurcation_data)
        }

        second_branch_idx<-grep("2",colnames(simulation_bifurcation))
        simulation_bifurcation_2<-simulation_bifurcation[,second_branch_idx]
        colnames(simulation_bifurcation_2)<-gsub("2","",colnames(simulation_bifurcation_2))
        simulation_bifurcation<-rbind(simulation_bifurcation[,setdiff(1:ncol(simulation_bifurcation),second_branch_idx)],simulation_bifurcation_2)
        simulation_bifurcation$id_parent<-rep(id_cut_nodes[num_of_descendant==2],2)



        #simulation_bifurcation$desc_length<-exp(simulation_bifurcation$desc_length)


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


      #neuron <- tortosity_integration_2(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node)

      is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
      #Añadir nuevos nodos a la neurona
      neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
      id_cut_nodes<-neuron$ids
      print(id_cut_nodes)
      max_id_node=max(id_cut_nodes)
      write_2_JSON(neuron,"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ongrowingneuron.json")

    }else{
      id_cut_nodes=c()
    }
    t<-t+1
  }

 # write_2_JSON(neuron,filename)
  #Fin repetir


  #neuron <- tortosify_neuron(neuron) DISABLED UNTIL TORTOSITY IMPLEMENTATION PHASE

  return(neuron)

}

