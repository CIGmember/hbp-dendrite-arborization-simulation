#' Train a model to simulate neurites.
#'
#' Train a model to simulate neurites. Learn two models, one to place the next node when the neurite continues and another one when the neurite bifurcates
#'
#' @param data is a dataframe which contains the features of each node
#' @param num_restart is an integer determining the number of restarts to learn the BN
#' @param max_iter maximum number of iterations to learn the BN
#'
#' @return a list with two models, the first one is the continuation model and the second one the bifurcation model
#'
#' @example
#' path<-"/home/universidad/Documents/neuron_data/datos/All"
#' data<-get_features_files(path,60)
#' data<-compute_clusters(data)
#' simulation_model<-compute_simulation_models(data)
compute_simulation_models<-function(data,num_restart=5,max_iter=Inf){
  cp_data<-data[,which(colnames(data)!="node_num_descendant")]
  cp_data$node_order<-as.factor(as.numeric(as.character(cp_data$node_order)))
  cp_data$desc_longer<-as.factor(as.numeric(as.character(cp_data$desc_longer)))
  #cp_data$dendrite_diameter<-as.factor(as.numeric(as.character(cp_data$dendrite_diameter)))

  continue_data<-cp_data[which(data$node_num_descendant==1),]
  bifurcation_data<-cp_data[which(data$node_num_descendant==2),]

  first_desc<-c("desc_azimuth_angle","desc_elevation_angle","desc_length")
  second_desc<-c("desc_azimuth_angle2","desc_elevation_angle2","desc_length2")

  data_names<-colnames(cp_data)
  non_desc_var<-setdiff(data_names,c(first_desc,second_desc,"node_num_descendant","desc_longer"))#,"dendrite_diameter"))

  factor_var<-intersect(data_names,c("node_order","subtree_max_order","subtree_min_order","subtree_no_bif","subtree_terminals","desc_longer"))
  continuous_var<-setdiff(data_names,factor_var)
  black_list<-expand.grid(continuous_var,factor_var)
  black_list<-unique(rbind(black_list,expand.grid(first_desc,non_desc_var)))

  continue_data<-continue_data[,!colnames(continue_data)%in%c("desc_azimuth_angle2","desc_elevation_angle2","desc_length2","desc_longer")]
  black_list_c<-black_list[which(!black_list[,1]%in%c(second_desc,"desc_longer") & !black_list[,2]%in%c(second_desc,"desc_longer")),]
  model_continue<-list()
  model_continue$structure<-bnlearn:::hc(continue_data,blacklist=black_list_c,restart=num_restart,max.iter = max_iter)
  model_continue$params<-bn.fit(model_continue$structure,continue_data)

  black_list_b<-as.matrix(expand.grid(c(first_desc,second_desc),non_desc_var))
  black_list_b<-unique(rbind(black_list_b,expand.grid(continuous_var,factor_var)))
  model_bifurcation<-list()
  model_bifurcation$structure<-bnlearn:::hc(bifurcation_data,blacklist=black_list_b,restart=num_restart,max.iter = max_iter)
  model_bifurcation$params<-bn.fit(model_bifurcation$structure,bifurcation_data)

  return(list(continue=model_continue,bifurcation=model_bifurcation))
}


compute_clusters<-function(data){
  temp_d<-data[data$desc_length>0 & data$desc_length2>0,]
  cluster<-Mclust(temp_d$desc_length+temp_d$desc_length2)
  data$dendrite_diameter<-rep(1,nrow(data))
  data$dendrite_diameter[data$desc_length>0 & data$desc_length2>0]<-cluster$classification
  return(data)
}




#' Extract a dataset of paired sets of coordinates of the nodes of first branches and their root nodes in the soma
#'
#'
#' @param file_path is a directory path that only contains neurons files with the compatible extensions (.swc,.DAT,.json)
#' @param eps is the parameter controlling the "level of detail" for the points to be extracted from the neuron. eps= 60
#' will output the neuron's "skeleton" with roughly one node per branch, while eps=0 will produce branches with multiple nodes and
#' visible tortuosity.
#' @return A dataframe containig paired sett of coordinates
#'
#' @example
#' path<-"/home/universidad/Documents/neuron_data/datos/All"
#' first_bif_pairs <- extract_first_node_coordinates(path,eps=eps)
extract_first_node_coordinates<-function(file_path,eps){

  path2files<-file.path(file_path,list.files(file_path))
  data<-data.table()
  count<-1
  for(file_path in path2files)
  {
    neuron<-neuro_converter(file_path,eps=eps)
    if(is.null(neuron$data$neurons$neurites)){
      last <- lapply(neuron$data$neurons[[1]]$neurites[[1]]$tree$children, function(x) c(x$nodes[[1]], id2 = x$root[1], x2 = x$root[2],y2 = x$root[3], z2 = x$root[4], r2 = x$root[5]))
          }
    else{
    last <- lapply(neuron$data$neurons$neurites[[1]]$tree$children, function(x) c(x$nodes[[1]], id2 = x$root[1], x2 = x$root[2],y2 = x$root[3], z2 = x$root[4], r2 = x$root[5]))
    }
    last <- lapply(last, function(x) if(length(x$id)>1){}else{x})
    last <- rbindlist(last)
    data <- rbind(data,last)
  }

  return(data)

}




#' Train the first bifurcation model with a dataset of paired sets of coordinates
#'
#'
#' @param first_bif_pairs is a dataframe containing paired sets of coordinates
#' of the nodes of first branches and their root nodes in the soma
#' @return a model
#'
#' @example
#' path<-"/home/universidad/Documents/neuron_data/datos/All"
#' first_bif_pairs <- extract_first_node_coordinates(path,eps=eps)
#' train_first_node_coordinates(first_bif_pairs)
train_first_node_coordinates<- function(first_bif_pairs){

  model_first_bif <- list()
  traindata <- first_bif_pairs[,c(2,3,4,7,8,9)]
  model_first_bif$structure <- bnlearn::hc(traindata,restart=5,max.iter = Inf)
  model_first_bif$params <- bn.fit(model_first_bif$structure,traindata)

  return(model_first_bif)

}



#' Train all models with a new dataset of neurons
#'
#'
#' @param file_path is a directory path that only contains neuron's files with the compatible extensions (.swc,.DAT,.json)
#' @param eps is the parameter controlling the "level of detail" for the points to be extracted from the neuron. eps= 60
#' will output the neuron's "skeleton" with roughly one node per branch, while eps=0 will produce branches with multiple nodes and
#' visible tortuosity.
#'
#' @return This function simply stores the newly trained models in .rda files.
#'
#' @example
#' path<-"/home/universidad/Documents/neuron_data/datos/All"
#' mega_retrain(path,eps=eps)
mega_retrain <- function(file_path,eps=60){
nodes_data_retrain <-get_features_files(file_path,eps)
model_first_bif<-extract_first_node_coordinates(file_path,eps)
model_first_bif_retrained <- train_first_node_coordinates(model_first_bif)
save(model_first_bif_retrained,file="./model_first_bif_retrained.rda")
desc_model_retrained<-train_num_descendant_model(nodes_data_retrain)
save(desc_model_retrained, file = "./desc_model_retrained.rda")
simulation_model_retrained <-compute_simulation_models(nodes_data_retrain,num_restart = 5,max_iter = Inf)
save(simulation_model_retrained,file = "./simulation_model_retrained.rda")
}




#Experimental area, tortosity at work.
compute_neo_tortosity_model <- function(node_branch_dataset_nt){
  #desc_length is still logaritmical, i have to undo that.


  num_restart = 5
  max_iter = Inf
  D <- length(node_branch_dataset_nt[1,])
  prov <- node_branch_dataset_nt




  node_branch_dataset_nt <- node_branch_dataset_nt[-which(node_branch_dataset_nt$node_num_descendant ==3),]#eliminate trifurcations
  node_branch_dataset_nt[is.na(node_branch_dataset_nt)] <- 0



  for(i in 1:D){
    c <- class(node_branch_dataset_nt[[i]])
    if(strcmp("factor",c)){
      el <- node_branch_dataset_nt[[i]][1]
      class(node_branch_dataset_nt[[i]])<-"numeric"
      el2 <- node_branch_dataset_nt[[i]][1]

      if(min(levels(el)) == 0){
        node_branch_dataset_nt[[i]]<-node_branch_dataset_nt[[i]] - (1)
      }
    }
  }



  data<- node_branch_dataset_nt
  cp_data<-data[,which(colnames(data)!="node_num_descendant")]
  #cp_data$node_order<-as.factor(as.numeric(as.character(cp_data$node_order)))
  #cp_data$desc_longer<-as.factor(as.numeric(as.character(cp_data$desc_longer)))
  #cp_data$dendrite_diameter<-as.factor(as.numeric(as.character(cp_data$dendrite_diameter)))

  continue_data<-cp_data[which(data$node_num_descendant!=2),]
  bifurcation_data<-cp_data[which(data$node_num_descendant==2),]

  first_desc<-c("desc_azimuth_angle","desc_elevation_angle","desc_length")
  #second_desc<-c("desc_azimuth_angle2","desc_elevation_angle2","desc_length2")

  data_names<-colnames(cp_data)
  non_desc_var<-setdiff(data_names,c(first_desc,second_desc,"node_num_descendant","desc_longer"))#,"dendrite_diameter"))

  factor_var<-intersect(data_names,c("node_order","subtree_max_order","subtree_min_order","subtree_no_bif","subtree_terminals","desc_longer"))

  continuous_var<-data_names
  black_list<-expand.grid(continuous_var,factor_var)
  black_list<-unique(rbind(black_list,expand.grid(first_desc,non_desc_var)))

  #continue_data<-continue_data[,!colnames(continue_data)%in%c("desc_azimuth_angle2","desc_elevation_angle2","desc_length2","desc_longer")]
  black_list_c<-black_list[which(!black_list[,1]%in%c(second_desc,"desc_longer") & !black_list[,2]%in%c(second_desc,"desc_longer")),]
  model_continue<-list()
  model_continue$structure<-bnlearn:::hc(continue_data,blacklist=black_list_c,restart=num_restart,max.iter = max_iter)
  model_continue$params<-bn.fit(model_continue$structure,continue_data)

  black_list_b<-as.matrix(expand.grid(c(first_desc,second_desc),non_desc_var))
  black_list_b<-unique(rbind(black_list_b,expand.grid(continuous_var,factor_var)))
  model_bifurcation<-list()
  model_bifurcation$structure<-bnlearn:::hc(bifurcation_data,blacklist=black_list_b,restart=num_restart,max.iter = max_iter)
  model_bifurcation$params<-bn.fit(model_bifurcation$structure,bifurcation_data)

  neo_tortosity_model<-list(continue=model_continue,bifurcation=model_bifurcation)
  save(neo_tortosity_model,file='./data/neo_tortosity_model.rda')

  return(neo_tortosity_model)

}
#newdataset but this time we pick the first node of the branch and using that as evidence we calculate the parameters of normal distributions to sample from
node_branch_dataset_neotortosity <- function(nodes_data_eps_0,nodes_data_eps_60){
  data <- nodes_data_eps_0
  data60 <- nodes_data_eps_60
  data60$desc_length <- exp(data60$desc_length)

  #oldcont = 1
  cont = 1
  col60 <- data60[,c(14,15)]
  col0 <- data[,c(14,15)]

  #experimentation
  cont_branches = 1
  del_entries <- which(col60$node_num_descendant==1 & col60$node_order != 0)
  col602 <- col60[-del_entries,]
  data602 <- data60[-del_entries,]

  branches_separation <- list()
  for(j in 1:length(data$node_num_descendant)){
    el60 <- col602[cont_branches,]
    el0 <- col0[j,]
    #entrenar el modelo y aplicarlo naturalmente dentro de tortosity.
    #hay que dejar esto listo ya, que falta por hacer todo lo que sigue del doctorado.

    if(el0$node_order ==0){
      tranch <- data[cont:j,]
      cont = cont+1
      branches_separation <- append(branches_separation, list(tranch))
      cont_branches = cont_branches + 1
    }
    else{
      if(sum(el0$node_num_descendant != 1)){
        tranch <- data[cont:j,]
        cont = j+1
        branches_separation <- append(branches_separation, list(tranch))
        cont_branches = cont_branches + 1
      }

    }
  }

  N60 <- length(data602[,1])
  D60 <- length(data602[1,])
  N <- length(data[,1])
  D <- length(data[1,])
  el_ant = 0
  el = 0
  cont_ant=1
  cont = 2

  v_0 <- 1:D*0
  n_0 <- names(data)
  n_1 <- c("N_nodes","length.mean","length.sigma","azimuth.mean", "azimuth.sigma","elevation.mean","elevation.sigma")

  n_01 <- c(n_0,n_1)
  ln_1 <- length(n_1)
  ln_01 <- length(n_01)



  branch_node_tort_rel <- data.frame(matrix(rep(1:ln_1*0,N60),nrow=N60,ncol=ln_1))

  #branch_node_tort_rel[1:N60,1:D60] <- data60


  names(branch_node_tort_rel) <- n_1

  #data_child <- data.frame(length.sigma = 0,length.mean=0,n_nodes=0,total_length=0,azimuth.sigma=0, azimuth.mean=0,)#los campos del punto inicial
  #tambien vamos a calcular el punto final?

  for(i in 1:N60){
    #punto inicial aqui
    el<-branches_separation[[i]]
    target_azimuth <-el$desc_azimuth_angle
    target_elevation <- el$desc_elevation_angle
    target_length <- el$desc_length

    fit_azimuth <- fitdistr(target_azimuth,densfun = "normal")
    fit_elevation <- fitdistr(target_elevation,densfun="normal")
    fit_length <- fitdistr(target_length,densfun = "normal")
    N_nodes <- length(el[,1])
    #Node_order <- el$node_order
    el21<- c("N_nodes" = N_nodes,"length.mean" = fit_length$estimate[1], "length.sigma"=fit_length$estimate[2],"azimuth.mean"=fit_azimuth$estimate[1],"azimuth.sigma"=fit_azimuth$estimate[2],"elevation.mean"=fit_elevation$estimate[1],"elevation.sigma"=fit_elevation$estimate[2])
    branch_node_tort_rel[i,] <- el21
  }
  branch_node_tort_rel<-cbind(data602,branch_node_tort_rel)



  return(branch_node_tort_rel)

}
node_branch_dataset_neotortosity_2 <- function(nodes_data_eps_0,nodes_data_eps_60){
  data <- nodes_data_eps_0
  data60 <- nodes_data_eps_60
  data60$desc_length <- exp(data60$desc_length)

  #oldcont = 1
  cont = 1
  col60 <- data60[,c(14,15)]
  col0 <- data[,c(14,15)]

  #experimentation
  cont_branches = 1
  del_entries <- which(col60$node_num_descendant==1 & col60$node_order != 0)
  col602 <- col60[-del_entries,]
  data602 <- data60[-del_entries,]

  branches_separation <- list()
  for(j in 1:length(data$node_num_descendant)){
    el60 <- col602[cont_branches,]
    el0 <- col0[j,]
    #entrenar el modelo y aplicarlo naturalmente dentro de tortosity.
    #hay que dejar esto listo ya, que falta por hacer todo lo que sigue del doctorado.

    if(el0$node_order ==0){
      tranch <- data[cont:j,]
      cont = cont+1
      branches_separation <- append(branches_separation, list(tranch))
      cont_branches = cont_branches + 1
    }
    else{
      if(sum(el0$node_num_descendant != 1)){
        tranch <- data[cont:j,]
        cont = j+1
        branches_separation <- append(branches_separation, list(tranch))
        cont_branches = cont_branches + 1
      }

    }
  }

  N60 <- length(data602[,1])
  D60 <- length(data602[1,])
  N <- length(data[,1])
  D <- length(data[1,])
  el_ant = 0
  el = 0
  cont_ant=1
  cont = 2

  v_0 <- 1:D*0
  n_0 <- names(data)
  n_1 <- c("N_nodes","length.mean","length.sigma","azimuth.mean", "azimuth.sigma","elevation.mean","elevation.sigma")

  n_01 <- c(n_0,n_1)
  ln_1 <- length(n_1)
  ln_01 <- length(n_01)



  branch_node_tort_rel <- data.frame(matrix(rep(1:ln_1*0,N60),nrow=N60,ncol=ln_1))

  #branch_node_tort_rel[1:N60,1:D60] <- data60


  names(branch_node_tort_rel) <- n_1

  #data_child <- data.frame(length.sigma = 0,length.mean=0,n_nodes=0,total_length=0,azimuth.sigma=0, azimuth.mean=0,)#los campos del punto inicial
  #tambien vamos a calcular el punto final?

  for(i in 1:N60){
    #punto inicial aqui
    el<-branches_separation[[i]]
    target_azimuth <-el$desc_azimuth_angle
    target_elevation <- el$desc_elevation_angle
    target_length <- el$desc_length

    fit_azimuth <- fitdistr(target_azimuth,densfun = "normal")
    fit_elevation <- fitdistr(target_elevation,densfun="normal")
    fit_length <- fitdistr(target_length,densfun = "normal")
    N_nodes <- length(el[,1])
    #Node_order <- el$node_order
    el21<- c("N_nodes" = N_nodes,"length.mean" = fit_length$estimate[1], "length.sigma"=fit_length$estimate[2],"azimuth.mean"=fit_azimuth$estimate[1],"azimuth.sigma"=fit_azimuth$estimate[2],"elevation.mean"=fit_elevation$estimate[1],"elevation.sigma"=fit_elevation$estimate[2])
    branch_node_tort_rel[i,] <- el21
  }
  branch_node_tort_rel<-cbind(data602,branch_node_tort_rel)



  return(branch_node_tort_rel)

}





