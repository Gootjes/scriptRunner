
#' @title Detect scripts in a folder
#' @importFrom glue glue
#' @export
detect_scripts <- function(base_path, exclude_status = c("LOCKED"), recursive = TRUE) {

  dirs <- c()

  for(d in list.dirs(path = base_path, recursive = recursive)) {
    if (file.exists(glue("{d}/{opt('marker_filename')}"))) {
      if(length(exclude_status) > 0) {
        s <- get_script_status(d, all = TRUE)[exclude_status]
        if(!any(s, na.rm = T)) {
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
