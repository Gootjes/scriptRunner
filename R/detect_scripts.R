
#' @title Detect scripts in a folder
#' @export
detect_scripts <- function(base_path) {
  Filter(x = list.dirs(path = base_path),
         f = function(d) {
           file.exists(glue("{d}/{opt('marker_filename')}"))
         })
}
