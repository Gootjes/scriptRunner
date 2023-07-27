
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
#' @importFrom glue glue_collapse
#' @importFrom options opt
#' @importFrom yaml as.yaml
#' @importFrom filelock lock
#' @importFrom filelock unlock
#' @importFrom withr defer
#' @export
make_script <- function(expr, name, path, seed, tee = FALSE, ...) {
  the_call <- match.call()
  the_expr <- the_call[[2]]

  context <- future::getGlobalsAndPackages(the_expr, substitute = T, envir = parent.frame(), ...)

  if(!missing(seed)) {
    string_seed <- glue("set.seed({seed})")
  } else {
    string_seed <- ""
  }

  string_lock <- deparse(substitute({

    .srGoalPost <- FALSE

    .sr_filelock_lock <- filelock::lock(lock_filename, exclusive = TRUE, timeout = 1000*1)

    if(is.null(.sr_filelock_lock)) {
      stop("Failed to get a lock on LOCK")
    }

  }, env = list(lock_filename = opt('lock_filename'))))

  string_unlock <- deparse(substitute({
    .srGoalPost <- TRUE

    cat("scriptRunner: Reached end of script", "\n")
  }))

  string_sink <- deparse(substitute({

    .srSinkOutput <- file(output_log_filename, open = 'wt')
    .srSinkMessage <- file(message_log_filename, open = 'wt')

    sink(file = .srSinkOutput, split = tee)
    sink(file = .srSinkMessage, type = "message")

    cat("scriptRunner: start script", "\n")

    cat(Sys.getpid(), file = lock_filename)

    #suppressMessages({
      withr::defer({
        # cat("scriptRunner: on.exit", "\n")

        suppressWarnings({
          sink(type = "message")
          sink()
        })

        #close(.srSinkOutput)
        #close(.srSinkMessage)

        if(.srGoalPost) {
          cat(Sys.getpid(), file = completed_filename)
        } else {
          cat(Sys.getpid(), file = halted_filename)
        }

        filelock::unlock(.sr_filelock_lock)
        file.remove(lock_filename)
      })
    #})

  }, env = list(
    tee = tee,
    lock_filename = opt('lock_filename'),
    output_log_filename = opt('output_log_filename'),
    message_log_filename = opt('message_log_filename'),
    completed_filename = opt('completed_filename'),
    halted_filename = opt('halted_filename')
  )))

  string_library <- glue("library({context$packages})")
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

  if(file.exists(opt('lock_filename'))) {
    stop("Cannot make script. Script already exists and is locked. Is it running?")
  }

  glue_collapse(c(
    # "(function() {",
    # "",
    string_lock,
    "",
    "",
    string_sink,
    "",
    "",
    "cat('scriptRunner: Loading libraries', '\n')",
    string_library,
    "",
    "",
    "cat('scriptRunner: Setting seed', '\n')",
    string_seed,
    "",
    "",
    "cat('scriptRunner: Executing code', '\n')",
    string_expr,
    "",
    "cat('scriptRunner: Finished executing code', '\n')",
    "",
    "",
    string_unlock,
    ""
    # "",
    # "})()"
  ), sep = '\n') -> body

  cat(body, file = opt('script_filename'))

  cat(as.yaml(list(FileVersion = "0.1.0",
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
