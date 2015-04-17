#' Remove all the files in a path of a git repository
#' 
#' Note that the removal is not staged
#'@inheritParams read_delim_git
#'@inheritParams base::list.files
#'@export
remove_files_git <- function(path, pattern = NULL, repo.path){
  to.remove <- list_files_git(
    path = path, 
    pattern = pattern, 
    repo.path = repo.path, 
    full.names = TRUE
  )
  
  success <- file.remove(to.remove)
  if(length(success) > 0 && !all(success)){
    stop("Error cleaning existing files in the git repository. Repository: '", repo.path, "', Path: '", path, "', pattern: '", pattern, "'")
  }
}
