#' @title new_dockerfile
#' @description Starter dockerfile
#' @author Joe Peskett
#' @export
#'
new_dockerfile <- function(){
  return(c())
}

#' @title step_CUSTOM
#' @description add CUSTOM step if commands are not covered in this package
#' @author Joe Peskett
#' @param dockerfile A dockerfile - note that this will not check to see if you have a valid dockerfile!
#' @param command Command to execute
#' @param args These should be formatted in a list
#' @export

step_CUSTOM <- function(dockerfile, command, args){
  if (class(args) != 'list'){
    stop('args should be a list')
  }
  return(
    append(
      dockerfile,
      paste(command,
            unlist(args)
      )
    )
  )
}


#' @title step_FROM
#' @description add a FROM step, usually used for selecting a base image
#' @author Joe Peskett
#' @param dockerfile A dockerfile - note that this will not check to see if you have a valid dockerfile!
#' @param image_name The name of the image to build from
#' @param image_tag The tag of the image. Defaults to latest
#' @export

step_FROM <- function(dockerfile, image_name, image_tag = "latest"){
  # add new line to text file with 'FROM image_name:image_tag
  # if image tag is omitted then just use latest
  return(
    step_CUSTOM(dockerfile,
                command = 'FROM',
                args = list(image_name, image_tag))
  )
}

#' @title step_COPY
#' @description add COPY step
#' @author Joe Peskett
#' @param dockerfile A dockerfile - note that this will not check to see if you have a valid dockerfile!
#' @param local_path File path to copy
#' @param dockerfile_path Where to copy to in the dockerfile
#' @export

step_COPY <- function(dockerfile, local_path, dockerfile_path){
  return(
    step_CUSTOM(dockerfile,
                command = 'COPY',
                args = list(local_path, dockerfile_path))
  )
}


#' @title step_RUN
#' @description add a RUN step in your dockerfile
#' @param dockerfile A dockerfile - note that this will not check to see if you have a valid dockerfile!
#' @param args A list of arguments
#' @author Joe Peskett
#' @export
step_RUN <- function(dockerfile, args){
  if (class(args) != 'list'){
    stop('args should be a list')
  }
  return(
    step_CUSTOM(dockerfile,
                command = 'RUN',
                args = args)
  )
}

#' @title step_CMD
#' @description add a CMD step in your dockerfile
#' @param dockerfile A dockerfile - note that this will not check to see if you have a valid dockerfile!
#' @param args A list of arguments to use as part of your CMD step
#' @author Joe Peskett
#' @export
step_CMD <- function(dockerfile, args){
  if (class(args) != 'list'){
    stop('args should be a list')
  }
  return(
    step_CUSTOM(dockerfile,
                command = 'CMD',
                args = args)
  )
}

#' @title writeDockerfile
#' @description Writes the dockerfile to file
#' @author Joe Peskett
#' @param dockerfile final dockerfile
#' @param file where to write the file
#' @export

writeDockerfile <- function(dockerfile, file){
  writeLines(dockerfile, file)
  return(
    paste('Dockerfile saved to:', file)
         )
}
