#' Check if the path a git repository
#' Checks is a '.git' subdirectory exists in 'path'
#' @param path the path to check
#' @param error What to do in case \code{path} is not a git repository. Throw an error when \code{error = TRUE}. Return \code{FALSE} when \code{error = FALSE}
#' @export
check_git_repo <- function(path, error = TRUE){
  path <- check_single_character(x = path, name = "path")
  error <- check_single_logical(x = error, name = "error")
  
  full.path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if(file_test("-d", paste0(full.path, "/.git"))){
    return(full.path)
  }
  if(error){
    stop("'", full.path, "' is not a git repository")
  }
  return(FALSE)
}
