#include <Rcpp.h>


#include <neurostr/core/log.h>
#include <neurostr/io/parser_dispatcher.h>
#include <neurostr/io/JSONWriter.h>

#include <rapidjson/ostreamwrapper.h>
#include <rapidjson/writer.h>
#include <rapidjson/prettywriter.h>
#include <rapidjson/stringbuffer.h>
#include <iostream>

#include <neurostr/selector/neuron_selector.h>

using namespace Rcpp;
// [[Rcpp::plugins(cpp14)]]
namespace ns = neurostr::selector;
using namespace Rcpp;
namespace bg =        boost::geometry;
using point_type =    bg::model::point<float, 3, bg::cs::cartesian>;
template <typename T>
using const_selector_reference = std::reference_wrapper<const T>;
using const_node_reference  = const_selector_reference<neurostr::Node>;

// [[Rcpp::export]]
std::string c_neuro_converter2(std::string ifile, bool correct, float eps) {
  std::ostringstream oss;

  auto r = neurostr::io::read_file_by_ext(ifile);

  if(r->n_contours()>0)
  {
    //Get the id of the last node
    std::vector<neurostr::Node> soma_nodes;
    int init_id=0;
    //Get all the nodes in the neuron and search for the max id
    std::vector<const_node_reference> neuron_nodes=ns::neuron_node_selector(*(r->begin()));
    neurostr::Node::id_type max_id_node=0;
    for(auto it=neuron_nodes.begin();it!=neuron_nodes.end();++it)
    {
      if(max_id_node<it->get().id())
      {
        max_id_node=it->get().id();
      }
    }
    init_id=max_id_node;

    //Save all the nodes in the contours in an array of nodes
    auto it = r->contour_begin();
    /*for(auto it = r->contour_begin(); it != r->contour_end();++it)
    {*/
      for(auto it2 = it->begin() ;it2 != it->end();++it2)
      {
        neurostr::Node new_node(init_id,*it2,0.2);
        soma_nodes.push_back(new_node);
        init_id++;
      }
   // }



    // Simpify, correct and add the nodes in the contour to the soma
    for(auto it = r->begin(); it != r->end(); ++it){
      if(correct) it->correct();
      if(eps != 0.0 ){
        it->simplify(eps);
      }
      it->erase_apical();
      it->erase_axon();
      it->add_soma(soma_nodes);
      it->center();
    }

  }else{

    for(auto it = r->begin(); it != r->end(); ++it){
      if(correct) it->correct();
      if(eps != 0.0 ){
        it->simplify(eps);
      }
    }
  }

  r->erase_contour();
  neurostr::io::JSONWriter writer(oss);
  writer.write(*r);

  std::string features = oss.str();

  return features;
}
