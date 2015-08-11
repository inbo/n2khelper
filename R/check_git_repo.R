#' Check if the path a git repository
#' Checks is a '.git' subdirectory exists in 'path'
#' @param path the path to check
#' @param error What to do in case \code{path} is not a git repository. Throw an
#'    error when \code{error = TRUE}. Return \code{FALSE} when \code{error = FALSE}
#' @export
check_git_repo <- function(path, error = TRUE){
  error <- check_single_logical(x = error, name = "error")

  full.path <- check_path(path, type = "directory")
  git.path <- check_path(
    paste(full.path, ".git", sep = "/"),
    type = "directory",
    error = FALSE
  )
  if (is.logical(git.path)) {
    if (error) {
      stop("'", full.path, "' is not a git repository")
    }
    return(FALSE)
  }
  return(full.path)
}
