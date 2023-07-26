
#' @title Make a script that runs an expression
#' @description
#' A folder is created that contains the script code, and the variable environment
#' in which it is executed. This is deduced from the expression (\code{expr}) passed
#' to this function.
#'
#' @param expr The expression to evaluate in the script. Relevant global
#' and local variables, and packages are detected with \code{\link[future]{getGlobalsAndPackages}}.
#' Note that this means that \code{expr} should be able to run in the current session.
#' @param ... Other arguments passed to \code{\link[future]{getGlobalsAndPackages}}
#' @return The path to the folder that represents the script
#' @examples
#' path_to_script <- make_script({  }, name = 'test-1', path = 'test-scripts')
#'
#' run_script(path_to_script)
#'
#' @importFrom future getGlobalsAndPackages
#' @importFrom glue glue
#' @importFrom options opt
#' @export
make_script <- function(expr, name, path, seed, ...) {
  the_call <- match.call()
  the_expr <- the_call[[2]]

  context <- future::getGlobalsAndPackages(the_expr, substitute = T, envir = parent.frame(), ...)

  if(!missing(seed)) {
    string_seed <- glue("set.seed({seed})")
  } else {
    string_seed <- ""
  }

  string_lock <- deparse(substitute({
    cat(Sys.getpid(), file = lock_filename)
  }, env = list(lock_filename = opt('lock_filename'))))

  string_unlock <- deparse(substitute({
    file.remove(lock_filename)
  }, env = list(lock_filename = opt('lock_filename'))))

  string_sink <- deparse(substitute({

    .sinkOutput <- file(output_log_filename, open = 'wt')
    .sinkMessage <- file(message_log_filename, open = 'wt')

    sink(.sinkOutput, type = "output")
    sink(.sinkMessage, type = "message")

    on.exit({
      sink(type = "message")
      sink(type = "output")

      close(.sinkOutput)
      close(.sinkMessage)
    })

  }, env = list(
    output_log_filename = opt('output_log_filename'),
    message_log_filename = opt('message_log_filename')
  )))

  string_library <- paste("library(", context$packages, ")", sep = "")
  string_expr <- deparse(context$expr)

  script_path <- paste0(path, "/", name, "/")

  if(!dir.exists(script_path)) {
    d <- dir.create(script_path, recursive = T)
    if(d == FALSE) {
      stop("Failed to create directory: ", script_path)
    }
  }

  wd <- getwd()

  on.exit({
    setwd(wd)
  })

  setwd(script_path)

  cat(c(
    string_lock,
    "",
    "",
    string_sink,
    "",
    "",
    string_library,
    "",
    "",
    string_seed,
    "",
    "",
    string_expr,
    "",
    "",
    string_unlock
  ), sep = '\n', file = opt('script_filename'))

  cat(yaml::as.yaml(list(FileVersion = "0.1.0",
                         PackageVersion = packageVersion('scriptRunner'))), file = opt('marker_filename'))

  save(file = opt('environment_filename'),
       list = names(context$globals),
       envir = as.environment(context$globals))

  script_path
}

srGetGlobalsAndPackages <- function(expr) {
  match.call()
  future::getGlobalsAndPackages(expr = substitute(expr))
}
