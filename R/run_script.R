

#' @title Run script made with \code{\link{make_script}}
#' @param script_path Path to the folder of the script
#' @param wait Whether to wait for the script to complete (Default) or not
#' @examples
#' run_script(make_script({ ... }, name = 'test-1', folder = 'test-scripts'))
#'
#' @importFrom glue glue
#' @export
run_script <- function(script_path, wait = TRUE, rscript = NULL) {

  with_script(script_path, {

    if(file.exists(opt('lock_filename'))) {
      stop("Cannot run script. Script is locked. Is it already running?")
    }

    if(!file.exists(opt('script_filename'))) {
      stop("No script to run found!")
    }

    if(is.null(rscript)) {
      rscript <- file.path(R.home("bin"), 'Rscript')
    }

    ret <- system(glue("{rscript} --restore --save {opt('script_filename')}"), wait = wait)

    if(ret != 0) {
      warning("There was an error during execution", call. = F, immediate. = T)
    }

    ret
  })

}
