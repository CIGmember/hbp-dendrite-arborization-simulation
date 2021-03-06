#' Neuron Box Cutter
#' The neuron box cutter creates a virtual axis-aligned box with given min and max corners and uses it to "cut" the neuron.
#' That is, it creates virtual nodes at the intersection of the box with the neuron, adds an empty property with key "cut"
#' to all branches and nodes that lie outside the reconstruction.
#' It also adds two properties to the neuron "cutbox_min" and "cutbox_max", the box min and max corner points.
#' If infinite value is given for certain corner x,y,z value, it takes the neuron min/max value for that axis.
#'
#' @param json_info info about a reconstruction in JSON format
#' @param mins a vector of three values determining the min x,y,z coordinates used to delimit the box. When the value is Inf it takes the min value of that axis
#' @param max a vector of three values determining the max x,y,z coordinates used to delimit the box. When the value is Inf it takes the max value of that axis
#'
#' @return a data.frame with the neuron cutted by the limits defined in mins and maxs vectors
#'
#' @export
write_2_JSON<-function(json_info,path="~/datos/simpl.json")
{
  write(json_info$plain,path)
}
