# Building Components

#' @title break_function
#' @description builds callable structure from the function body,
#' @param function a function to be broken down
#' @return a string that will run in an R sesseion to
#' 1. Define the function,
#' 2. parse arguments to the function,
#' 3. return the outputs of t
#' @author Joe Peskett
#' @export
break_function <- function(func, check_output = FALSE){
  func_as_string <- capture.output(func)
  #remove bytecode
  func_as_string <- gsub("<bytecode: [[:alnum:]]*>",
                         replacement = "",
                         x = func_as_string)
  #Remove any hashed comments
  func_as_string <- gsub("#[[:print:]]*",
                         replacement = "",
                         x = func_as_string)
  #remove environment pointer if present
  func_as_string <- gsub("<environment: [[:print:]]*",
                         replacement = "",
                         x = func_as_string)
  func_as_string[1] <- paste("worker <-", func_as_string[1])
  if (check_output == TRUE){
    writeLines(func_as_string, 'see_here.R')
  }
  return(paste(func_as_string, '\n' ))
}

#' @title string_to_func
#' @description take a string and evaluate it to produce a function called 'worker'
#' @param func_as_string this will be the output of break_funcion
#' @author Joe Peskett
#' @export

string_to_func <- function(func_as_string){
  eval(
    parse(
      text = paste(func_as_string,
                   collapse = "")
    )
  )
}

#' @title function_to_interface
#' @description given a function, return it as a string
#' and create an interface that will take arguments when called from
#' terminal
#' @param function The function to break up
#' @author Joe Peskett
#' @export

function_to_interface <- function(func){
  func_string <- break_function(func = func, check_output = FALSE)
  func_args <- as.list(args(func))
  interface <- "commandArgs(TRUE)"
}


#' @title component_from_function
#' @description this function builds a Kubeflow Pipelines component from an R function.
#' Note that any packages required by the function, should be added into the
#' @author Joe Peskett
#' @param func an R function
#' @param base_image a base image to use for the component
#' @param component_output an optional argument to save the component to a YAML file
#' @export
component_from_function <- function(func, base_image, component_output_file = NULL){

  #Read the args of the function
  arg_list <- as.list(args(func))
  #Build the details of the
  name <- 'Pipeline Component'
  inputs <- list(list(name = 'input 1'),
                 list(name = 'input 2'),
                 list(name = 'input 3'))
  #Build implementation
  implementation <- list(
    container = list(
      image = base_image,
      command = list('R',
                     '-q', #Run quiet
                     '-e'#Execute the commanda
                     ),
      args = list()
    ))

  #Build the required yaml file
  component <- list(name = name,
                    inputs = inputs,
                    implementation = implementation)
  if (is.null(component_output_file) == FALSE){
    write_yaml(component, component_output_file)
  }
}
#' @title load_component_from_file
#' @description load a kubeflow pipelines component from file
#' @param component_path
#' @author Joe Peskett
#' @export
load_component_from_file <- function(component_path){
  # load the yaml file, prepare in the correct format to build a KFP .tag.gz
  return(component_def)
}

