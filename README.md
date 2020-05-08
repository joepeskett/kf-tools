# Kubeflow Tools (R)

This is a simple utility package to try and aid with building components for Kubeflow Pipelines in R. 

Kubeflow pipelines are essentially argo 

Requirements:

R Packages:

* `yaml` for exporting yaml files
* `argparse` for formatting the inputs to the component

Python Packages:

* If you want to compile a Kubeflow pipelines package, upload or run a package, you will need to have the `kfp` package installed, as we will use the command line tools installed alongside the python package. 

Docker:

* If you want to build your own docker images to use with the components, then you will need to 
