#' @title break_function
#' @description builds callable structure from the function body,
#' @param func a function to be broken down
#' @param check_output should we write the list out
#' @return a string that will run in an R sesseion to
#' 1. Define the function,
#' 2. parse arguments to the function,
#' 3. return the outputs of t
#' @author Joe Peskett
#' @import utils
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
#' @param func The function to break up
#' @param check_output should we save the output; for debugging purposes
#' @author Joe Peskett
#' @export

function_to_interface <- function(func, check_output = FALSE){
  func_string <- break_function(func = func, check_output = FALSE)
  func_args <- as.list(args(func))
  func_args <- func_args[lengths(func_args) != 0]
  n_args <- length(func_args)
  function_params <- paste0(names(func_args),
                      collapse = ',')
  parser <- paste(
    sapply(X = names(func_args),
           FUN = function(x){paste0("parser$add_argument('--",x,"',dest = '",x,"') \n ")}
           ), collapse  = '')
  #Convert the interface to using argparse
  interface <- paste0("
  library(argparse)
  if(length(args) != ",n_args+1,"){
    stop('Wrong number of args')
  }
  parser <- ArgumentParser()
  ",parser,"
  parser$add_argument('--return_output_path', dest = 'return_output_path')
  args <- parser$parse_args()
  for(i in seq(1, length(args), 1)){assign(names(args[i]), args[[i]])}
  `__return-output__` <- worker(",function_params,")
  directory <- dirname(args$return_output_path)
  if(dir.exists(directory)==FALSE){
    dir.create(directory,
               recursive = TRUE)
  }
  if(any(class(`__return_output__`) %in% c('character', 'numeric'))){
    writeLines(as.character(`__return_output__`), args$return_output_path)
  }else if(any(class(`__return_output__`) %in% c('data.frame','tibble'))){
    write.csv(`__return_output__`, args$return_output_path)
  }else{
    saveRDS(`__return_output__`, args$return_output_path)
    }
  "
  )
  if (check_output == TRUE){
    writeLines(text = interface, 'interface.R')
  }
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
#' @param component_output_file an optional argument to save the component to a YAML file
#' @import stats
#' @import utils
#' @export

component_from_function <- function(func, base_image,component_output_file = NULL) {
  #arg_names <- names(arg_list)[-length(arg_list)] # This could be removed.
  arg_list <- as.list(args(func))
  arg_list <- arg_list[lengths(arg_list) != 0]
  input_Values <- arg_list[!grepl('_inputPath', x = names(arg_list)) & !grepl('_outputPath', x = names(arg_list))]
  input_Paths <- arg_list[grepl('_inputPath', x = names(arg_list))]
  output_Paths <- arg_list[grepl('_outputPath', x = names(arg_list))]
  #ins and outputs
  input_names <- c(names(input_Values), names(input_Paths))
  input_list <- lapply(input_names, function(x){setNames(list(x), nm = 'name')})
  output_names <- c(names(output_Paths), 'return_output_path')
  output_list <- lapply(output_names, function(x){setNames(list(x), nm = 'name')})
  #Here we need to sub out our existing logic for our new logic for building the argumentslike so
  # - --parameter-name,
  # - inputValue: parameter-name
  #--------------
  full_args_list <- c(
    sapply(names(input_Values), function(x){c(paste0('--',x), list(list(inputValue = x)))}),
    sapply(names(input_Paths), function(x){c(paste0('--',x), list(list(inputPath = x)))}),
    sapply(c(names(output_Paths), 'return_output_path'), function(x){c(paste0('--',x), list(list(outputPath = x)))})
  )
  name <- as.character(quote(func))
  description <- 'This is a stock description, we have not added this parameter in yet'
  #Build R commands
  function_call <- function_to_interface(func = func, check_output = FALSE)
  commands <- paste(function_call[[1]], function_call[[2]])
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
    yaml::write_yaml(component, component_output_file)
  }
  return(full_args_list)
}
