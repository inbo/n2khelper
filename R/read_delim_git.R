#' read a tab delimited file from a git repository
#' @param file the name of the file
#' @param path the subdirectory of the file relative to the root of the git repository
#' @param repo.path path of the git repository. Defaults to rawdata.path
#' @export
read_delim_git <- function(file, path, repo.path = rawdata.path){
  file <- check_single_character(x = file, name = "file")
  path <- check_single_character(x = path, name = "path")
  repo.path <- check_single_character(x = repo.path, name = "repo.path")
  repo.path <- normalizePath(repo.path, winslash = "/", mustWork = FALSE)
  if(!is_git_repo(path = repo.path)){
    stop(repo.path, " is not a git repository")
  }
  
  filename <- paste(repo.path, path, file, sep = "/")
  if(file_test("-f", filename)){
    return(read.delim(filename))
  } else {
    return(FALSE)
  }
}
