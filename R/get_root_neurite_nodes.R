#Model to predict if a neurite has been cutted
get_root_neurite_nodes<-function(json_info)
{
  root_nodes_coords<-c_get_root_neurite_nodes(json_info)

  # normalize_vectors<-sweep(root_nodes,1,sqrt(rowSums(root_nodes^2)),"/")
  # PCA_neurites<-normalize_vectors%*%prcomp(normalize_vectors)$rotation
  # angle<- atan2(PCA_neurites[,2],PCA_neurites[,1])

  return(root_nodes_coords)
}

