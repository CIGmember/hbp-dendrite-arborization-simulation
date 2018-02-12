

#' Given the paths of real and synthetic neurons. Performs a series of validation tests
#'
#' Given a terminal node, simulate the bifurcation of the dendrite
#'
#' @param file_path_real is a directory containing only neuron files with the compatible extensions (.swc,.DAT,.json)
#' @param file_path_synthetic is a directory containing only neuron files with the compatible extensions (.swc,.DAT,.json)
#' @param eps is the parameter controlling the "level of detail" for the points to be extracted from the neuron. eps= 60
#' will output the neuron's "skeleton" with roughly one node per branch, while eps=0 will produce branches with multiple nodes and
#' visible tortuosity.
#'
#' @return a file containing the results of the tests
#'
#' @example
#' path_real<-"/home/universidad/Documents/neuron_data/datos/All"
#' path_synthetic<-"/home/universidad/Documents/neuron_data/datos/Generated"
#' Testing(path_real,path_synthetic,eps=eps)


Testing <- function(file_path_real,file_path_synthetic,eps){


  final_path <-file_path_synthetic

  path2files<-file.path(file_path_real,list.files(file_path_real))
  path2sfiles <-file.path(file_path_synthetic,list.files(file_path_synthetic))
  data<-data.table()
  count<-1

  real_nodes_data <- list()
  synth_nodes_data <- list()
  real_branches_data <- list()
  synth_branches_data<- list()

  l_p2f <-length(path2files)
  l_p2sf <-length(path2sfiles)

  if(l_p2f>l_p2sf){
    L_path<-l_p2sf
  }
  else{
    L_path <- l_p2f
  }

  for(i in 1:L_path)
  {

    file_path_real <- path2files[i]
    file_path_synthetic<- path2sfiles[i]

    neuron_real<-neuro_converter(file_path_real,eps=eps)
    neuron_synth <-neuro_converter(file_path_synthetic)

    features_nodes_real<-rbindlist(node_feature_extractor(neuron_real$plain))
    features_nodes_synthetic <- rbindlist(node_feature_extractor(neuron_synth$plain))

    real_nodes_data <-append(real_nodes_data,list(features_nodes_real))
    synth_nodes_data <-append(synth_nodes_data,list(features_nodes_synthetic))
  }

  real_nodes_data2 <-rbindlist(real_nodes_data)
  synth_nodes_data2 <- rbindlist(synth_nodes_data)

  filtering = 1
  if(filtering){
    real_nodes_data2 <- real_nodes_data2[real_nodes_data2$path_to_root!=0,]
    synth_nodes_data2 <- synth_nodes_data2[synth_nodes_data2$path_to_root!=0,]

    real_nodes_data2 <- real_nodes_data2[,-c(22,24)]
    synth_nodes_data2 <- synth_nodes_data2[,-c(22,24)]
  }

  univariate_tests=0

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

  #p_value_KL_multivariate <- KL.div.test(real_nodes_data2,synth_nodes_data2)

  #Building a simple classifier

  bundled_nodes <- rbind(real_nodes_data2,synth_nodes_data2)
  L_R_N <- length(real_nodes_data2[[1]])
  L_S_N <- length(synth_nodes_data2[[1]])

  synth <- rep(0,L_S_N)
  real  <- rep(1,L_R_N)
  class_nodes <- c(real,synth)
  class<-data.table(class = class_nodes)
  bundled_nodes<- cbind(class,bundled_nodes)

  #Adapting the features
  attributes(bundled_nodes$class)$levels<-c("0","1")
  featured_data_nodes<-bundled_nodes[,colnames(bundled_nodes)%in%c("class","azimuth_angle","compartment_length","distance_to_root","elevation_angle","length_to_brach_root","node_num_descendant","node_order","node_to_brach_root_dist","path_to_root,subtree_box_volume","subtree_length","tortuosity"),with=F]
  featured_data_nodes$class<-factor(featured_data_nodes$class,levels=as.numeric(attributes(table(featured_data_nodes$class))$dimnames[[1]]))
  featured_data_nodes$node_order<-factor(featured_data_nodes$node_order,levels=as.numeric(attributes(table(featured_data_nodes$node_order))$dimnames[[1]]))
  training_selection_nodes <- sample(1:length(class_nodes),round(length(class_nodes)*0.75),replace=FALSE)
  training_data_nodes <- featured_data_nodes[training_selection_nodes,]
  BN_nodes <- learn_BN(training_data_nodes)

  #Prediction time
  testing_data_nodes <- setdiff(1:length(bundled_nodes[[1]]),training_selection_nodes)
  testing_data_nodes <- featured_data_nodes[testing_data_nodes,]
  nodes_predictions <- pred_BN_2(BN_nodes,testing_data_nodes)
  acc_nodes_predictions <- sum(abs(as.numeric(as.character(nodes_predictions$truth))== as.numeric(as.character(nodes_predictions$resp))))/length(nodes_predictions[[1]])#0.7076862


 #Report
  report = data.table(KStwosampleNodes = p_values_ks_nodes, KLtwosampleNodes = p_values_kl_nodes, WilcoxtwosampleNodes = p_values_wil_nodes, AccuracyNodes = acc_nodes_predictions)
  write.matrix(report, file=paste(final_path,"/report",sep=""))

  return(report)

}


KL.div.test<- function(real_nodes_data2,synth_nodes_data2){

  Kl_test_reference_values <- (1:100)*0

  for(i in 1:100){
    b_1 <- sample(1:length(real_nodes_data2[[1]]), size = length(real_nodes_data2[[1]]),replace=TRUE)
    b_1 <- real_nodes_data2[b_1,]
    b_2 <- sample(1:length(real_nodes_data2[[1]]), size = length(real_nodes_data2[[1]]),replace=TRUE)
    b_2 <- real_nodes_data2[b_2,]
    Kl_test_reference_values[i] <- KL.div.mult.implemented(b_1,b_2)
    #<- KL.divergence(as.matrix(b_1),as.matrix(b_2),k=100,algorithm = "cover_tree")[100]
  }

  threshold = max(Kl_test_reference_values)*0.95
  kldivermul <- KL.div.mult.implemented(real_nodes_data2,synth_nodes_data2)
  #<- KL.divergence(as.matrix(real_nodes_data2),as.matrix(synth_nodes_data2),k=100,algorithm = "cover_tree")[100]
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
KL.div.mult.implemented<- function(b_1,b_2){

  N_p = length(b_1[[1]])
  N_q = length(b_2[[1]])

  n = length(b_1[1,])

  div = 0

  D_p = normalize_datamult(b_1)
  D_q = normalize_datamult(b_2)

  D_p_unique = unique(normalize_datamult(b_1))
  D_q_unique = unique(normalize_datamult(b_2))


  for(i in 1:N_p){
    el = D_p[i,]
    #D_p_no_i <- rbind(el,D_p_unique)
    #D_p_no_i <- unique(D_p_no_i)
    #D_p_no_i <- D_p_no_i[-1,]

    #D_p_no_i = D_p[-i,]
    #no_i = which(sapply(1:N_p, function (x) identical(D_p[x,],el)))
    #D_p_no_i =D_p[-no_i,]
    NN_Ec <-Distance_for_KNN_test(el, D_p_unique)
    NN_Ec2 <-Distance_for_KNN_test(el, D_q_unique)

    NN_Ec<-sort(NN_Ec,decreasing = FALSE)[2]
    NN_Ec2 <- min(NN_Ec2)



    div = div + log(NN_Ec2/NN_Ec)
  }

  result = (n/N_p)*div + log(N_q/(N_p-1))

  return(result)

}
normalize_datamult<- function(datamult){

  N<-length(datamult[[1]])
  L <- length(datamult[1,])
  for(i in 1:L){
    el = datamult[[i]]

    min_el = min(el)
    el = el - min(el)
    max_el = max(el)
    el = el/(max_el)
    datamult[[i]] = el
    if(max_el == Inf){
      motherfucker = 1
    }
    if(sum(is.na(max_el)) > 0){
      motherfucker=1
    }
    if(min_el == -Inf){
      motherfucker = 1
    }
  }
  return(datamult)
}
NN_Eucl_dist<- function(el,data){
  # distances <- vapply(data, function(x) x)
  N = length(data[[1]])
  distances = 1:N*0
  for(i in 1:N){
    el2 = data[i,]
    res = el - el2
    res = res^2
    res =sum(res)
    res = sqrt(res)
    distances[i]= res
  }

  min_dist = min(distances)
  member = which(distances == min_dist)[1]

  return(list(member,min_dist))

}
