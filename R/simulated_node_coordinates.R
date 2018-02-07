#' Compute the cartesian coordinates of the simulated nodes
#' The node feature extractor computes a set of prebuilt measures for each node in the reconstruction.
#' Measure values are not aggregated in any way, they are returned as-is (usually as vectors).
#' Optionally, it tries to correct errors in the reconstruction.
#'
#' @param json_info info about a reconstruction in JSON format
#' @param omit_apical a boolean value. If set, apical dendrite is not measured
#' @param omit_axon a boolean value. If set, axon is not measured
#' @param omit_dend a boolean value. If set, dendrites are not measured
#' @param correct a boolean value. The converter calls the correct method on each neuron in the reconstruction
#' @param removeZjumps a boolean value. Remove jumps over Z axis produced by the Z jumps in the microscopy imaging
#'
#' @return a data.frame containing the values measured for each neurite in the reconstruction
#'
#' @export
simulated_node_coordinates<-function(json_info, simulated_nodes, is_bifurcation,init_id, omit_apical=F, omit_axon=F, omit_dend=F, correct=F, removeZjumps=T)
{
  neuron_json <- c_simulated_position(json_info, simulated_nodes, is_bifurcation, init_id, omit_apical, omit_axon, omit_dend, correct, removeZjumps)
  neuron <- fromJSON(neuron_json[[1]])

  return(list(plain=neuron_json[[1]],data=neuron,ids=neuron_json[[2]]))
}
