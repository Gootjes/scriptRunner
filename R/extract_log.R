
#' @title Extract log from script
#' @param script_path Path to the script's directory
#' @return Character vector of log messages
#' @export
extract_log <- function(script_path, type = c('message', 'output')) {

  wd <- getwd()

  on.exit({
    setwd(wd)
  })

  setwd(script_path)

  if(type == 'message') {
    readLines(opt("message_log_filename"))
  } else if(type == 'output') {
    readLines(opt("output_log_filename"))
  } else {
    stop("Unknown log type")
  }

}
