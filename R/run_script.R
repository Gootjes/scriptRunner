

#' @title Run script made with \code{\link{make_script}}
#' @param script_path Path to the folder of the script
#' @param wait Whether to wait for the script to complete (Default) or not
#' @examples
#' run_script(make_script({ ... }, name = 'test-1', folder = 'test-scripts'))
#'
#' @export
run_script <- function(script_path, wait = TRUE) {

  wd <- getwd()

  on.exit({
    setwd(wd)
  })

  setwd(script_path)

  if(!file.exists(opt('script_filename'))) {
    stop("No script to run found!")
  }

  ret <- system(paste("Rscript.exe", "--restore", "--save", opt('script_filename')), wait = wait)

  if(ret != 0) {
    warning("There was an error during execution", call. = F, immediate. = T)
  }

  ret
}
