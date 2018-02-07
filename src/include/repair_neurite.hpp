#ifndef __REPAIR_NEURITE__
#define __REPAIR_NEURITE__


#include <neurostr/core/neuron.h>
#include <neurostr/core/node.h>

#include <neurostr/selector/selector.h>
#include <neurostr/selector/node_selector.h>
#include <neurostr/selector/neuron_selector.h>

void repair_neurite(neurostr::Neurite& n);
void repair_neuron(neurostr::Neuron& n);

#endif // __REPAIR_NEURITE__

