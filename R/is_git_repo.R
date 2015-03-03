#' It the path a git repository
#' Checks is a '.git' subdirectory exists in 'path'
#' @param path the path to check
#' @export
#' @return A logical vector wit the same length as path
is_git_repo <- function(path){
  path <- check_single_character(x = path, name = "path")
  file_test("-d", paste0(path, "/.git"))
}
