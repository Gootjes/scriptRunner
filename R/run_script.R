

#' @title Run script made with \code{\link{make_script}}
#' @param script_path Path to the folder of the script
#' @param rscript_binary Location of rscript binary
#' @param wait see ?system
#' @param ignore.stdout see ?system
#' @param ignore.stderr see ?system
#' @param show.output.on.console see ?system
#' @param timeout see ?system
#' @examples
#' run_script(make_script({ ... }, name = 'test-1', folder = 'test-scripts'))
#'
#' @importFrom glue glue
#' @export
run_script <- function(script_path,
                       rscript_binary = file.path(R.home("bin"), 'Rscript'),
                       wait = TRUE,
                       ignore.stdout = TRUE,
                       ignore.stderr = TRUE,
                       show.output.on.console = FALSE,
                       timeout = 0,
                       ...) {

  with_script(script_path, {

    if(file.exists(opt('lock_filename'))) {
      stop("Cannot run script. Script is locked. Is it already running?")
    }

    if(!file.exists(opt('script_filename'))) {
      stop("No script to run found!")
    }

    ret <- system(command = glue("{rscript_binary} --restore --save {opt('script_filename')}"),
                  intern = FALSE,
                  wait = wait,
                  ignore.stdout = TRUE,
                  ignore.stderr = TRUE,
                  show.output.on.console = FALSE,
                  timeout = 0,
                  ...)

    if(ret != 0) {
      warning("There was an error during execution", call. = F, immediate. = T)
    }

    ret
  })

}
