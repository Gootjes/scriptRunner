
get_own_filename <- function() {
  arg_prefix <- "--file="

  args <- commandArgs()

  cat(paste(args, collapse = "\n"), "\n")

  args <- Filter(function(a) {
    startsWith(a, arg_prefix)
  }, args)

  if (length(args) == 0) {
    return(NULL)
  }

  cat(args, "\n")

  strsplit(args[[1]], split = arg_prefix, fixed = T)[[1]][[2]]
}
