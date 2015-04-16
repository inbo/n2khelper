#' Remove all the files in a path of a git repository
#' 
#' Note that the removal is not staged
#'@inheritParams read_delim_git
#'@inheritParams base::list.files
#'@export
remove_files_git <- function(path, pattern = "^[0123456789].*\\.txt$", repo.path = rawdata.path){
  path <- check_single_character(x = path, name = "path")
  repo.path <- check_single_character(x = repo.path, name = "repo.path")
  repo.path <- normalizePath(repo.path, winslash = "/", mustWork = FALSE)
  if(!is_git_repo(path = repo.path)){
    stop(repo.path, " is not a git repository")
  }

  full.path <- paste(repo.path, path, sep = "/")
  full.path <- normalizePath(full.path, winslash = "/", mustWork = FALSE)
  if(file_test("-d", full.path)){
    file.remove(list.files(path = full.path, pattern = pattern, full.names = TRUE))
  } else {
    stop(full.path, " is not a directory")
  }
}
