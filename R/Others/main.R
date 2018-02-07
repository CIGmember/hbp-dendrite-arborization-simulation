###MAIN##
main<-function(seed=1)
{
  seed=1
  max_id_node=0; #Maximo id de los nodos que pertenecen a la neurona. Necesario para generar los nuevos ids de los nodos.

  set.seed(seed)
  #Aprender o precargar modelos para clasificar y simular nodos terminales
  desc_model<-neurostr::desc_model
  simulation_model<-neurostr::simulation_model

  #Leer neurona indicada por el usuario
  file_path =  "/home/pablo/Desktop/Simulacion-arborizaciones-basales/Experiments/ca1a.CNG.swc" #they look completely diffrent from our set
  # file_path<-"/home/universidad/Documents/neuron_data/datos/All/h213III1.DAT"
  #file_path <- "/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/HUMANOS_IIIyV/TodosDAT/H263III4.DAT"
  #file_path<-"/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/HUMANOS_IIIyV/TodosDAT/H264III10.DAT"
  #  file_path<-"/home/universidad/datos/Cortadas/human cing id if6 porta 2 sec1 cel20.DAT"

  #neuron<-neuro_converter(file_path,eps=60)
  neuron<-neuro_converter(file_path,eps=0)

  #Simular los nodos raíz de las neuritas basales de una neurona

  id_cut_nodes<-get_cut_nodes(neuron$plain)

  #neuron <- get_simulated_neuron()
  #id_cut_nodes <- neuron[[3]]

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
        neuron<-simulated_node_coordinates(neuron$plain, data.matrix(simulated_data), is_bifurcation,max_id_node)
        id_cut_nodes<-neuron$ids
        print(id_cut_nodes)
        max_id_node=max(id_cut_nodes)

    }else{
      id_cut_nodes=c()
    }
    t<-t+1
  }
  #Fin repetir
 dasd = 2

}
