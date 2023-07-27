
#' @title Remove a script (directory)s
#' @param script_path The script's directory
#' @param askConfirmation Whether to ask for confirmation
#' @importFrom glue glue
#' @export
remove_script <- function(script_path, askConfirmation = interactive(), force = FALSE) {

  if(!dir.exists(script_path)) {
    stop("Directory does not exist.")
  }

  if(!file.exists(glue("{script_path}/{opt('marker_filename')}"))) {
    stop("This directory does not appear to be a script directory")
  }

  if(file.exists(glue("{script_path}/{opt('lock_filename')}"))) {
    if(!force) {
      stop("Cannot remove script. Script directory is locked. Is it running? If you want to remove it anyway, use force = TRUE")
    }
  }

  unlink(script_path, recursive = T, force = T)
}
