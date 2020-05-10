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


#NOTE: add argparse in here

function_to_interface <- function(func, check_output = FALSE){
  func_string <- break_function(func = func, check_output = FALSE)
  func_args <- as.list(args(func))
  n_args <- length(func_args)-1
  arguments <- paste0(paste0('args[',1:n_args,']'),
                      collapse = ',')
  #Convert the interface to using argparse
  interface <- sprintf("
  library(argparse)
  args = commandArgs(TRUE)
  if(length(args) != %s){
    stop('Wrong number of args')
    }
  `__return-output__` <- worker(%s)
  directory <- dirname(args[length(args)])
  if(dir.exists(directory)==FALSE){
    dir.create(directory,
               recursive = TRUE)
  }
  if(any(class(`__return_output__`) %in% c('character', 'numeric'))){
    writeLines(as.character(`__return_output__`), output_location)
  }else if(any(class(`__return_output__`) %in% c('data.frame','tibble'))){
    write.csv(`__return_output__`, output_location)
  }else{
    saveRDS(`__return_output__`, output_location)
    }
  "
  , n_args+1, arguments)
  if (check_output == TRUE){
    writeLines(text = interface, 'interface.R')
  }
  return(list(paste(func_string,
                    collapse= ""),
              interface))
}

#' @title save_output
#' @description General function for saving outputs
#' @param output_object the object to be saved
#' @author Joe Peskett
#' @export
save_output <- function(output_object, output_location){
  object_type <- class(output_object)
  if(any(object_type %in% c('character', 'numeric'))){
    writeLines(as.character(output_object),
               con = output_location)
  }else if(any(object_type == 'data.frame')){
    write.csv(output_object,
              file = ouput_location)
  }else{
    saveRDS(object = output_object, file = output_location)
  }
}


#' @title format_output
#' @description formats the ouput of a pipeline component
#' @param output_list a list of the ouputs of a given function.
#' @author Joe Peskett
#' @export
format_output <- function(output_list){
  n_outputs <- length(output_list)

}

#' @title build_interface
#' @description build wrap an R function in an interface to be called from Command Line
#' @param func A function
#' @author Joe Peskett
#' @export
build_interface <- function(func){
  func_string <- break_function(func = func, check_output = FALSE)
  func_name <- as.character(quote(func))
  func_args <- as.list(args(func))
  n_args <- length(func_args)-1
  arguments <- paste0(paste0('args[',1:n_args,']'),
                      collapse = ',')
  interface <- sprintf(
    "library(argparse)
    parser = ArgumentParser(prog = %s, add_help = FALSE)
    "
  , func_name,)

  #Write outputs
  if (check_output == TRUE){
  writeLines(text = interface, 'interface.R')
  }
  #paste everything together as an element in a list
  return(list(paste(func_string,
                    collapse= ""),
              interface))

}

#' @title component_from_function
#' @description this function builds a Kubeflow Pipelines component from an R function.
#' Note that any packages required by the function, should be added into the
#' @details Components are built up of inputs and outputs which are used as argumens to an implementation of a container.
#' @author Joe Peskett
#' @param func an R function
#' @param base_image a base image to use for the component
#' @param outputs_list Should be a character vector representing the outputs of the component.`
#' @param component_output_file an optional argument to save the component to a YAML file
#' @export
#'
#'
#' write_yaml(sapply(args_vec, function(x){c(paste0('--',x), list(list(inputValue = x)))}), 'example.yaml')
#' sapply(args_vec, function(x){c(paste0('--',x), list(list(inputValue = x)))})
component_from_function <- function(func, base_image,component_output_file = NULL) {
  #arg_names <- names(arg_list)[-length(arg_list)] # This could be removed.
  arg_list <- as.list(args(func))
  input_Values <- arg_list[!grepl('_inputPath', x = names(arg_list)) & !grepl('_outputPath', x = names(arg_list))]
  input_Paths <- arg_list[grepl('_inputPath', x = names(arg_list))]
  output_Paths <- arg_list[grepl('_outputPath', x = names(arg_list))]
  #ins and outputs
  input_names <- c(names(input_Values), names(input_Paths))
  input_list <- lapply(input_names, function(x){setNames(list(x), nm = 'name')})
  output_names <- c(names(output_Paths))
  output_list <- lapply(output_names, function(x){setNames(list(x), nm = 'name')})
  #Here we need to sub out our existing logic for our new logic for building the argumentslike so
  # - --parameter-name,
  # - inputValue: parameter-name
  #--------------
  full_args_list <- c(
    sapply(names(input_Values), function(x){c(paste0('--',x), list(list(inputValue = x)))}),
    sapply(names(input_Paths), function(x){c(paste0('--',x), list(list(inputPath = x)))}),
    sapply(names(output_Paths), function(x){c(paste0('--',x), list(list(outputPath = x)))})
  )
  #input_val_args < lapply(arg_names, function(x){
  #  if(!grepl('_inputPath', x=x) & !grepl('_outputPath', x=x))setNames(list(x), nm = 'inputValue')
   # })
  #input_path_args <- lapply(arg_names, function(x){if(grepl('_inputPath',x = x)) setNames(list(x), nm = 'inputPath')})
  #output_path_args <- lapply(arg_names, function(x){if(grepl('_outputPath',x = x)) setNames(list(x),nm = 'outputPath')})
  #output_args <- lapply(outputs_list, function(x){setNames(list(x), nm = 'outputPath')})
  #arguments <- c(input_val_args, input_path_args, output_path_args, output_args)
  #arguments <- arguments[lengths(arguments) != 0]
  #n_outputs <- length(outputs_list) # This is the number of output arguments to create
  #Name and description for the component.
  name <- as.character(quote(func))
  description <- 'This is a stock description, we have not added this parameter in yet'
  #Build R commands
  function_call <- function_to_interface(func = func, check_output = FALSE)
  commands <- paste(function_call[[1]], function_call[[2]])
  #Build the output for return of the function:

  #Build implementation
  implementation <- list(
    container = list(
      image = base_image,
      command = list('R',
                     '-q', #Run quiet
                     '-e',#Execute the commanda
                     commands,
                     '--args'),
      args = unname(full_args_list)
    )
  )

  #Build the required yaml file
  component <- list(name = name,
                    inputs = input_list,
                    outputs = output_list,
                    implementation = implementation)
  if (is.null(component_output_file) == FALSE){
    write_yaml(component, component_output_file)
  }
  return(full_args_list)
}

#' @title load_component_from_file
#' @description load a kubeflow pipelines component from file
#' @param component_path location of the component to load
#' @author Joe Peskett
#' @export
load_component_from_file <- function(component_path){
  # load the yaml file, prepare in the correct format to build a KFP .tag.gz
  return(component_def)
}

