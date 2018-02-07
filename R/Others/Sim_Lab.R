#simulate_neuron<-function(seed=1)
simulate_neuron <- function(seed=1,filename)
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
  load("./data/tortosity_model.rda")#model_tortosity

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


        simulated_tortosity<-simulate_tortosity(model_tortosity,simulation_continue,continue_data)
      #  simulation_continue <- simulated_data

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


        simulated_bifurcation_tortosity <- simulate_tortosity(model_tortosity,simulation_bifurcation,bifurcation_data)


        if(nrow(simulation_data[num_of_descendant==1])>0)
        {
          simulated_data<-rbind(simulation_continue,simulation_bifurcation)
          simulated_tortosity <- append(simulated_tortosity, simulated_bifurcation_tortosity)
        }else{
          simulated_data<-simulation_bifurcation
          simulated_tortosity <- simulated_bifurcation_tortosity
        }
      }

      #simulated_data$desc_length<-exp(simulated_data$desc_length)
      #we probably need to recalculate this.

      #simulation bifurcation is from last iteration, needs to be cleaned for the new one, this may solve the issue if not we have to go inside the method to try to verify whats the exact
      #rows that need to be processed
      #On another topic, we have to solve the problem with the tortosity looking like a zig zag, maybe the distances problem is solved now but still, we need it to be right


      neuron <- tortosity_integration (neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node)

      #is_bifurcation<-c(rep(1,nrow(simulation_data[num_of_descendant==1])),rep(2,nrow(simulation_data[num_of_descendant==2])*2))-1
       #Añadir nuevos nodos a la neurona
      #neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)#this also needs reworking, as it needs to add known nodes to it and it assigns their ids, so i have to call it multiple times each time containing a known node
      id_cut_nodes<-neuron$ids
      print(id_cut_nodes)
      max_id_node=max(id_cut_nodes)

    }else{
      id_cut_nodes=c()
    }
    t<-t+1
  }

  write_2_JSON(neuron,filename)
  #Fin repetir
 return(neuron)

}


#here we repeat in a loop the integration of the different calculated dots for the tortosity model
#we have the original data from sergio and the expanded simulated tortosity data, conveniently
#separated into 2 variables. We know that the tortosity data uses the id of the sergio's simulated node as first id of our first tortosity simulated node,
#but the values of azimuth and elevation calculated for sergio are maintained for the first datapoint, while length is simulated, the rest of the points are built on top, but now
#we know we needed the output of simulated_node_coordinates to address them. Since we know that all simulated datapoints per branch are in a chain parent->child, we can then rewrite the
#ids at convenience so we can add the following ones through repeated alls to simulate_node_coordinates until all tortosity datapoints are included and the neuron is ready to receive more.
#is bifurcation will only be bifurcation for the datapoints that sergio calculated, the rest are 0s since they simply continue the
tortosity_integration <- function(neuron,simulation_data,simulated_tortosity,simulated_data,num_of_descendant,id_cut_nodes,max_id_node){



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
    card_ord$desc_length <- exp(card_ord$desc_length)


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

