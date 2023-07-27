
#' @title Extract log from script
#' @param script_path Path to the script's directory
#' @return Character vector of log messages
#' @export
extract_log <- function(script_path, type = c('message', 'output')) {

  with_script(script_path, {
    if(file.exists(opt('lock_filename'))) {
      warning("Script directory is locked. Log may not be complete yet.", call. = F, immediate. = T)
    }

    if(type == 'message') {
      readLines(opt("message_log_filename"))
    } else if(type == 'output') {
      readLines(opt("output_log_filename"))
    } else {
      stop("Unknown log type")
    }
  })

}
