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

simulate_neuron_2 <- function(seed=1)
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

       # simulated_tortosity<-simulate_tortosity(model_tortosity,simulation_continue,continue_data)
       # simulated_tortosity<-simulate_empty_tortosity(branch_length_nodes_model,simulation_continue,t+2)

      #  simulated_tortosity_2[[1]] <- simulated_tortosity[[1]]
      #  simulated_tortosity <- simulated_tortosity_2

        #simulation_continue <- simulated_data

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




Testing <- function(file_path_real,file_path_synthetic,eps,Gen){
#real in .dat, synthethic in .json
#/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/Real
#/home/pablo/Desktop/Simulacion-arborizaciones-basales/Data/Synthetic

  final_path <-file_path_synthetic
  path2files<-file.path(file_path_real,list.files(file_path_real))

  #we give the option to generate a synthetic dataset that matches the real one in number and tortosity
  if(Gen){
    file_path_synthethic <- GenerateSyntheticData(file_path_synthethic,eps)
  }

  path2sfiles <-file.path(file_path_synthetic,list.files(file_path_synthetic))
  data<-data.table()
  count<-1


  real_nodes_data <- list()
  synth_nodes_data <- list()
  real_branches_data <- list()
  synth_branches_data<- list()


  for(i in 1:length(path2files))
  {
    file_path_real <- path2files[i]
    file_path_synthetic<- path2sfiles[i]

    neuron_real<-neuro_converter(file_path_real,eps=eps)
    neuron_synth <-neuro_converter(file_path_synthetic)

    features_nodes_real<-rbindlist(node_feature_extractor(neuron_real$plain))
    features_nodes_synthetic <- rbindlist(node_feature_extractor(neuron_synth$plain))

    real_nodes_data <-append(real_nodes_data,list(features_nodes_real))
    synth_nodes_data <-append(synth_nodes_data,list(features_nodes_synthetic))

    #features_branches_real <- branch_feature_extractor(neuron_real$plain)
    #features_branches_synthetic <- branch_feature_extractor(neuron_synth$plain)

    #features_branches_real <- features_branches_real$measures
    #features_branches_synthetic <- features_branches_synthetic$measures

    #real_branches_data <-append(real_branches_data,list(features_branches_real))
    #synth_branches_data <- append(synth_branches_data,list(features_branches_synthetic))

  }

  real_nodes_data2 <-rbindlist(real_nodes_data)
  synth_nodes_data2 <- rbindlist(synth_nodes_data)
  #real_branches_data2 <- rbindlist(real_branches_data)
  #synth_branches_data2 <- rbindlist(synth_branches_data)


  #perhaps a preprocessing phase here to eliminate rows with NANs and constant values. Yes indeed

  filtering = 1

if(filtering){

  #real_branches_data2 <- real_branches_data2[!is.na(real_branches_data2$local_bifurcation_angle),]
  #real_branches_data2 <- real_branches_data2[!is.na(real_branches_data2$hill_taper_rate),] #in a general version these two should go in a method that cleans all rows with nans

  #synth_branches_data2 <- synth_branches_data2[!is.na(synth_branches_data2$local_bifurcation_angle),]
  #synth_branches_data2 <- synth_branches_data2[!is.na(synth_branches_data2$hill_taper_rate),] #in a general version these two should go in a method that cleans all rows with nans

  real_nodes_data2 <- real_nodes_data2[real_nodes_data2$path_to_root!=0,]
  synth_nodes_data2 <- synth_nodes_data2[synth_nodes_data2$path_to_root!=0,]

  real_nodes_data2 <- real_nodes_data2[,-c(22,24)]
  synth_nodes_data2 <- synth_nodes_data2[,-c(22,24)]
}






    #now we can do the testing. Kolmogorov Smirnof 2 sample test for every pair, and include new descriptors and train the classifier, #we could split
  #the method here in order to benefit the UI informing the user of the already completed operations, but for now we keepo it this way

  p_values_ks_nodes <- rep(0,length(real_nodes_data2[1,]))
  p_values_kl_nodes <- rep(0,length(real_nodes_data2[1,]))
  p_values_wil_nodes <- rep(0,length(real_nodes_data2[1,]))


  for(j in 1:length(real_nodes_data2[1,])){
    col_real <- real_nodes_data2[[j]]
    col_synth <- synth_nodes_data2[[j]]
    p_value <- ks.test(col_real,col_synth)
    p_values_ks_nodes[j]<-p_value$p.value
    p_values_kl_nodes[j] <- kldiver.test(col_real,col_synth)
    wilcox<-wilcox.test(col_real,col_synth, paired=FALSE)
    p_values_wil_nodes[j] <- wilcox$p.value
  }


  p_value_KL_multivariate <- KL.div.test(real_nodes_data2,synth_nodes_data2)


 # p_values_ks_branches <- rep(0,length(real_branches_data2))
#  p_values_kl_branches <- rep(0,length(real_branches_data2))
#  p_values_wil_branches <- rep(0,length(real_branches_data2))
#  for(j in 1:length(real_branches_data2[1,]))
#  {
#    col_real <- real_branches_data2[[j]]
#    col_synth <- synth_branches_data2[[j]]
#    p_value <- ks.test(col_real,col_synth)
#    p_values_ks_branches[j]<-p_value$p.value
#    p_values_kl_branches[j] <- kldiver.test(col_real,col_synth)
#    wilcox<-wilcox.test(col_real,col_synth, paired=FALSE)
#    p_values_wil_branches[j] <- wilcox$p.value
#  }

#Visual inspection seems to point out to equivalence but KS tests do not, perhaps because the look for the exact distribution and not an approximation, the more data the more
#"exigent" they become

#Building a simple classifier

  bundled_nodes <- rbind(real_nodes_data2,synth_nodes_data2)
#  bundled_branches <- rbind(real_branches_data2,synth_branches_data2)
  L_R_N <- length(real_nodes_data2[[1]])
  L_S_N <- length(synth_nodes_data2[[1]])
#  L_R_B <- length(real_branches_data2[[1]])
#  L_S_B <- length(synth_branches_data2[[1]])

  synth <- rep(0,L_S_N)
  real  <- rep(1,L_R_N)
  class_nodes <- c(real,synth)
  class<-data.table(class = class_nodes)
  bundled_nodes<- cbind(class,bundled_nodes)

  #adapting the features

  attributes(bundled_nodes$class)$levels<-c("0","1")
  featured_data_nodes<-bundled_nodes[,colnames(bundled_nodes)%in%c("class","azimuth_angle","compartment_length","distance_to_root","elevation_angle","length_to_brach_root","node_num_descendant","node_order","node_to_brach_root_dist","path_to_root,subtree_box_volume","subtree_length","tortuosity"),with=F]
  featured_data_nodes$class<-factor(featured_data_nodes$class,levels=as.numeric(attributes(table(featured_data_nodes$class))$dimnames[[1]]))
  featured_data_nodes$node_order<-factor(featured_data_nodes$node_order,levels=as.numeric(attributes(table(featured_data_nodes$node_order))$dimnames[[1]]))
  training_selection_nodes <- sample(1:length(class_nodes),round(length(class_nodes)*0.75),replace=FALSE)
  training_data_nodes <- featured_data_nodes[training_selection_nodes,]
  BN_nodes <- learn_BN(training_data_nodes)


#  synth <- rep(0,L_S_B)
#  real  <- rep(1,L_R_B)
  #class_branches <- c(real,synth)
 # class<-data.table(class = class_branches)
#  bundled_branches<- cbind(class,bundled_branches)

  #adapting the features


  #aqui hacer algo similar a lo que se ha hecho arriba, probablemente haya sido la simple eliminacion de features toxicas y lo que pasa cuando fiteas una normal con una sample
  #constante en esta implementacion

#  bundled_branches <- bundled_branches[!is.na(bundled_branches$local_bifurcation_angle),]
#  bundled_branches <- bundled_branches[!is.na(bundled_branches$hill_taper_rate),] #in a general version these two should go in a method that cleans all rows with nans
#  attributes(bundled_branches$class)$levels<-c("0","1")
#  featured_data_branches<-bundled_branches[,colnames(bundled_branches)%in%names(bundled_branches)[c(1,5,8,9,11,14,15,16,18,19,20)],with=F]
#  featured_data_branches$class<-factor(featured_data_branches$class,levels=as.numeric(attributes(table(featured_data_branches$class))$dimnames[[1]]))
#  featured_data_branches$centrifugal_order<-factor(featured_data_branches$centrifugal_order,levels=as.numeric(attributes(table(featured_data_branches$centrifugal_order))$dimnames[[1]]))
#  bundled_branches$class<-factor(bundled_branches$class,levels=as.numeric(attributes(table(bundled_branches$class))$dimnames[[1]]))
#  training_selection_branches <- sample(1:length(bundled_branches[[1]]),round(length(bundled_branches[[1]])*0.75),replace=FALSE)
#  training_data_branches <- featured_data_branches[training_selection_branches,]
 # BN_branches <- learn_BN(training_data_branches)

  #Prediction time
  testing_data_nodes <- setdiff(1:length(bundled_nodes[[1]]),training_selection_nodes)
  testing_data_nodes <- featured_data_nodes[testing_data_nodes,]
  #testing_data_nodes <- testing_data_nodes[,-c(1)]

  nodes_predictions <- pred_BN_2(BN_nodes,testing_data_nodes)

 # testing_data_branches <-setdiff(1:length(bundled_branches[[1]]),training_selection_branches)
#  testing_data_branches <- featured_data_branches[testing_data_branches,]
#  branches_predictions <- pred_BN_2(BN_branches,testing_data_branches)
  #accuracy measurements to compare the real ones with the synthethic ones

  acc_nodes_predictions <- sum(abs(as.numeric(as.character(nodes_predictions$truth))== as.numeric(as.character(nodes_predictions$resp))))/length(nodes_predictions[[1]])#0.7076862

  #  acc_branches_predictions <- sum(abs(as.numeric(as.character(branches_predictions$truth))==as.numeric(as.character(branches_predictions$resp))))/length(branches_predictions[[1]])#0.9966583... fuck

#  report = data.table(KStwosampleNodes = p_values_ks_nodes,KStwosampleBranches = p_values_ks_branches, KLtwosampleNodes = p_values_kl_nodes, KLtwosampleBranches = p_values_kl_branches, WilcoxtwosampleNodes = p_values_wil_nodes, WilcoxtwosampleBranches = p_values_wil_branches,AccuracyNodes = acc_nodes_predictions,AccuracyBranches = acc_branches_predictions)
  report = data.table(KStwosampleNodes = p_values_ks_nodes, KLtwosampleNodes = p_values_kl_nodes, WilcoxtwosampleNodes = p_values_wil_nodes, AccuracyNodes = acc_nodes_predictions)

  write.table(report, file=paste(final_path,"/report",sep=""))


}


KL.div.test<- function(real_nodes_data2,synth_nodes_data2){

  Kl_test_reference_values <- (1:100)*0

  for(i in 1:100){
    b_1 <- sample(real_nodes_data2, size = length(real_nodes_data2[,1]),replace=TRUE)
    b_2 <- sample(real_nodes_data2, size = length(creal_nodes_data2[,1]),replace=TRUE)
    Kl_test_reference_values[i]<- KL.divergence(as.matrix(b_1),as.matrix(b_2),k=100,algorithm = "cover_tree")[100]
  }

   threshold = max(Kl_test_reference_values)*0.95
   kldivermuk <- KL.divergence(as.matrix(real_nodes_data2),as.matrix(synth_nodes_data2),k=100,algorithm = "cover_tree")[100]
   aceptance <- kldivermul < threshold
   return(aceptance)

}

kldiver.test <- function(col_real,col_synth){

vals_real <- unique(col_real)
vals_synth <- unique(col_synth)

N_vals_real <- length(vals_real)
N_vals_synth <- length(vals_synth)

if(N_vals_real > 50){
  N_vals_real = 50
}

if(N_vals_synth > 50){
  N_vals_synth = 50
}

Kl_test_reference_values <- (1:100)*0

for(i in 1:100){
  b_1 <- sample(col_real, size = length(col_real),replace=TRUE)
  b_2 <- sample(col_real, size = length(col_real),replace=TRUE)
  bins_not_ready = 1
  #ensure finite KL divergence
  while(bins_not_ready){
  bins_b_1 <- entropy::discretize(b_1,numBins=N_vals_real)
  bins_b_2 <- entropy::discretize(b_2,numBins=N_vals_synth)

  if(sum(bins_b_1==0) + sum(bins_b_2==0)){
    N_vals_real = N_vals_real - 1
    N_vals_synth = N_vals_synth - 1
  }
  else{
    bins_not_ready = 0
  }
  }
  if(N_vals_real != N_vals_synth){
    N_vals_real <- min(c(N_vals_real,N_vals_synth))
    N_vals_synth <- min(c(N_vals_real,N_vals_synth))
    bins_b_1 <- entropy::discretize(b_1,numBins=N_vals_real)
    bins_b_2 <- entropy::discretize(b_2,numBins=N_vals_synth)
  }
  Kl_test_reference_values[i]  <- KL.plugin(bins_b_1, bins_b_2, unit=c("log", "log2", "log10"))

}

threshold = max(Kl_test_reference_values)*0.95

bins_real <- entropy::discretize(col_real,numBins=N_vals_real)
bin_synth <- entropy::discretize(col_synth,numBins=N_vals_synth)
kldiver <- KL.plugin(bins_real, bin_synth, unit=c("log", "log2", "log10"))

aceptance <- kldiver < threshold

return(aceptance)

}

