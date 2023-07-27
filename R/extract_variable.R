
#' @title Extract variable from a completed script
#' @description
#' This function assumes that the script has completed execution without error.
#'
#' @param script_path The path to the script
#' @param variable_name The name of the variable(s) to extract, character vector
#' @return A named list with names equal to \code{variable_name}, or if \code{drop == TRUE}
#' and variable_name is of length 1, the value of the variable.
#'
#' @importFrom options opt
#' @export
extract_variable <- function(script_path, variable_name, drop = length(variable_name) == 1) {

  with_script(script_path, {

    if(file.exists(opt('lock_filename'))) {
      stop("Cannot extract variable. Script did not complete execution or there were fatal errors during execution.")
    }

    e <- new.env(parent = emptyenv())
    load(opt('environment_filename'), envir = e)

    if(drop == TRUE) {
      if(length(variable_name) != 1) {
        stop("drop = TRUE but length of 'variable_name' is not equal to 1. Cannot return more than 1 value directly.")
      }
      e[[variable_name]]
    } else {
      as.list(e)[variable_name]
    }

  })

}
