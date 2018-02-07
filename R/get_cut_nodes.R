#' Check terminal nodes to select those that have been cut and return the id of that nodes
#'
#' The function computes the 3D convex hull of the terminal nodes. Then search the normal faces that points to the same direction.
#' If there are several coplanar faces there is a cut and all that terminal nodes are delimiting a cut.
#'
#' @param json_info info about a reconstruction in JSON format
#'
#' @return an array with the id of the terminal nodes that belong to the cut
#'
#' @export
get_cut_nodes<-function(json_info)
{
  terminal_nodes <- c_get_terminal_nodes(json_info)

  terminal_nodes_coords<-terminal_nodes[,1:3]
  terminal_nodes_id<-terminal_nodes[,4]
  #Compute convex hull of the terminal points
  convex_hull<-list()
  convex_hull$vb<-t(cbind(terminal_nodes_coords,1))
  convex_hull$it<-t(convhulln(terminal_nodes_coords))
  class(convex_hull)<-"mesh3d"

  #Orientante all the face normals to point out the mesh
  convex_hull<-vcgClean(convex_hull,7)

  face_normals<-t(facenormals(convex_hull)$normals)
  cl<-dbscan(face_normals,0.3,minPts=1)
  cut_face<-which(table(cl$cluster)>5)
  idx_cut_faces<-which(cl$cluster%in%cut_face)

  node_projection<-vcgClostKD(terminal_nodes_coords,convex_hull)
  nodes_inside_convex_hull<-which(node_projection$quality!=0)
  close_face_nodes_inside<-node_projection$faceptr[nodes_inside_convex_hull]

  cut_nodes_inside<-nodes_inside_convex_hull[which(close_face_nodes_inside%in%idx_cut_faces)]
  cut_nodes_convex_hull<-which(unlist(lapply(vcgVFadj(convex_hull),function(x){any(x%in%idx_cut_faces)})))

  cut_nodes<-c(cut_nodes_convex_hull,cut_nodes_inside)

  return(terminal_nodes_id[cut_nodes])
}
