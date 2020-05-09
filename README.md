# Kubeflow Tools (R)

This is a simple utility package to try and aid with building components for Kubeflow Pipelines in R. 

## Installation

```
library(devtools)
install_github('joepeskett/kf-tools')
```

Note that this will **not** handle the installation of the required python packages. 

## Example Usage:

> Define an R function:

```
add <- function(a,b){return(a+b)}
```
> Build a Kubeflow pipeline component yaml

```
library(kubeflowTools)
component_from_function(add, base_image = r:3.6,component_output_file = 'my_first_component.yaml')
```

> Load component when building a pipeline (in Python using the KFP SDK)

```
import kfp.components as comp
comp.load_component_from_file('my_first_component.yaml)
```
Depending on how useful this tool is, we may look at building functions for building pipeline packages into this R package also. 


### Requirements:

R Packages:

* `yaml` for exporting yaml files
* `argparse` for formatting the inputs, outputs and arguments to the component. 

Python Packages:

* If you want to compile a Kubeflow pipelines package, upload or run a package, you will need to have the `kfp` package installed, as we will use the command line tools installed alongside the python package. 

Docker:

* If you want to build your own docker images to use with the components, then you will need to install docker.
