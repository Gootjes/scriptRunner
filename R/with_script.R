
with_script <- function(script_path, expr, envir = parent.frame()) {

  the_call <- match.call()
  the_expr <- the_call[[3]]

  if (!dir.exists(script_path)) {
    stop("Path does not exist.")
  }

  wd <- getwd()

  on.exit({
    setwd(wd)
  })

  setwd(script_path)

  if (!file.exists(opt('marker_filename'))) {
    stop("Not a script directory.")
  }

  # I use environment() here, but it also moves up to the upper environment?
  evalq(expr = expr, envir = environment(), enclos = parent.frame())

}
