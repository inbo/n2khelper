#' Get the SHA of the files at the HEAD
#' @param file the name of the files
#' @param path the subdirectory of the file relative to the root of the git repository
#' @param repo.path path of the git repository. Defaults to rawdata.path
#' @export
#' @importFrom git2r repository odb_blobs head branch_target
git_sha <- function(file, path, repo.path = rawdata.path){
  repo.path <- check_single_character(x = repo.path, name = "repo.path")
  repo.path <- normalizePath(repo.path, winslash = "/", mustWork = FALSE)
  if(!is_git_repo(path = repo.path)){
    stop(repo.path, " is not a git repository")
  }
  file <- check_character(x = file, name = "file")
  path <- check_single_character(x = path, name = "path")
  
  repo <- repository(repo.path)
  blobs <- odb_blobs(repo)
  blobs <- blobs[
    blobs$path == path & 
    blobs$name %in% file &
    blobs$commit == branch_target(head(repo))
    , 
  ]
  return(blobs)
}
