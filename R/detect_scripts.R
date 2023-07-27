
#' @title Detect scripts in a folder
#' @param base_path Path to start search from
#' @param include_status Character vector describing which status to filter on.
#'  Use \code{NULL} to include all
#' @param recursive Whether to look recursively in folders
#' @importFrom glue glue
#' @export
detect_scripts <- function(base_path, include_status = c("NOT_STARTED",
                                                         "LOCKED",
                                                         "HALTED",
                                                         "COMPLETED"), recursive = TRUE) {

  dirs <- c()

  for(d in list.dirs(path = base_path, recursive = recursive)) {
    if (file.exists(glue("{d}/{opt('marker_filename')}"))) {
      if(length(include_status) > 0) {
        s <- get_script_status(d, all = TRUE)[include_status]
        if(any(s, na.rm = T)) {
          dirs <- c(dirs, d)
        }
      } else {
        dirs <- c(dirs, d)
      }
    }
  }

  if(is.null(dirs)) {
    invisible()
  } else {
    sort(dirs)
  }

}
