
parse_args <- function(attach = F) {
  trailing_args <- commandArgs(trailingOnly = T)

  l <- list()

  for(a in trailing_args) {
    sa <- strsplit(a, split = "=", fixed = T)[[1]]
    if (length(sa) == 2) {
      lhs <- sa[[1]]
      rhs <- sa[[2]]

      an <- suppressWarnings({
        as.numeric(rhs)
      })

      if(!is.na(an)) {
        rhs <- an
      }

      l[[lhs]] <- rhs
    }
  }

  if (attach) {
    attach(l, name = "args")
  }

  l
}
