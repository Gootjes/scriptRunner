
#' @title Remove a script (directory)s
#' @param script_path The script's directory
#' @param askConfirmation Whether to ask for confirmation
#' @importFrom glue glue
#' @export
remove_script <- function(script_path, askConfirmation = interactive()) {

  if(!file.exists(glue("{script_path}/{opt('marker_filename')}"))) {
    stop("This directory does not appear to be a script directory")
  }

  unlink(script_path, recursive = T, force = T)
}
