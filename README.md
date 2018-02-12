# hbp-dendrite-arborization-simulation


## Description
This shinny application is designed to model the simulation and visualization of 3d reconstructed dendrite trees.


## Functionality
It enables synthetic neurons file generation via user interface and visualization via the neuroviewer attached screen. Additionally, its possible to retrain the internal models of the application using a new dataset, and use it to create new simualtions. Finally, the app enables for performance testing and comparisons between populations of neurons.

##Prerequirements
R enviroment (Rstudio recomended as it was used for its development).


 
## Install
This shinny application relies strongly on neuroSTR. It is strongly recommended to install neuroSTR first as all the dependencies are required to run the application. 

Installation instructions for neuroSTR can be found here:

https://github.com/lrodriguezlujan/neurostr

Moreover it requires the following list of packages (tested with current versions as of Feb 2018):

Rcpp, rjson, Rvcg, geometry, Morpho, data.table, dbscan, bnlearn, msm, Hmisc, mclust, MASS,rbmn, pracma, compoisson, devtools,mvCircular,stats,spdep,jsonlite, FastKNN

Installation instructions for package mvCircular (non-CRAN) can be found here:

https://github.com/lrodriguezlujan/mvcircular



## Use cases

### Neuron simulation
Using either the already trained models embbeded into the application or the resulting ones from retraining the model with the user's data, its possible to generate a number of simulated neurons as .json files in an specified directory.


### Model retraining
Providing the model with a dataset large enough to properly train the models, its possible to obtain a new set of models that can be used for the simulation of a population of neurons. The resulting simulations can be later validated against the source population of real neurons with the validation tool included in the application.

### 3D reconstructed neurons validation
This step can be done post simulation or it could be used independently of our model to validate two populations of 3d reconstructed neurons contained in two different directories. Given the flexibility of neuroSTR to admit different file formats, the neurons of the two different directories need not to share the same file extension. 

Importantly, the eps parameter will be taken into account for the reading of the neuron populations.

### Neuron visualization

The right panel of the shinny app includes NeuroViewer, a 3D neuron visualizer that will allow the user to inspect visually all simulated representations as well as neuron reconstructed files compatible with neuroviewer.

## Notes
The GUI of the shinny application is not polished, so the user has to make sure the inputs are correct. For example, when introducing directories, we encourage the user to double check if they only contain neuron files and that the directory is a valid one. The results from testing output a report file in the directory of the simulated population of neurons. In order to use it again in the same session, this file should be removed from that directory.

### Notes related on tortuosity simulation and eps parameter
In its current state, the application cannot handle well full tortuosity, that is, an eps parameter of 0. It is possible to train the models, but the results are not intended as the final solution for tortuosity neuron's simulation. It is for now adviced to only use the eps value of 60.

On the file "tortuosity.R",the user can find an alternative method, "simulate_neuron_5()" that includes a tortuosity simulation fully trained model (and while not commented, the methods that generate the dataset for trainning, train the model and simulate predictions are included) that generates neurons with tortuosity at eps value 0. It is advised to take the tortuosity simulations with caution. The recommended use is for a more "neuroscience-friendly" visualization of the final simulation.


