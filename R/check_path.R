#' check if a path is an exisiting file or directory
#' @export
#' @param path the path of the directory or file name
#' @param type either "file" or "directory"
#' @inheritParams check_dataframe_variable
#' @importFrom assertthat assert_that is.flag is.string noNA
#' @importFrom utils file_test
check_path <- function(path, type = c("file", "directory"), error = TRUE) {
  assert_that(is.string(path), noNA(path))
  type <- match.arg(type)
  assert_that(is.flag(error), noNA(error))

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
