#' check if a path is an exisiting file or directory
#' @export
#' @param path the path of the directory or file name
#' @param type either "file" or "directory"
#' @inheritParams check_dataframe_variable
check_path <- function(path, type = c("file", "directory"), error = TRUE){
  type <- match.arg(type)
  path <- check_single_character(path, name = "path")
  error <- check_single_logical(error, name = "error")

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (type == "file") {
    if (file_test("-f", path)) {
      return(path)
    }
  } else {
    if (file_test("-d", path)) {
      return(path)
    }
  }
  if (error) {
    stop("'", path, "' is not a ", type)
  }
  return(FALSE)
}
