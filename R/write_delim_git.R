#' write a dataframe as a tab delimited file to a git repository and stage it
#' The existing file will be overwritten.
#' @param x the data.frame
#' @param file the name of the file
#' @param path the subdirectory of the file relative to the root of the git repository
#' @param repo.path path of the git repository. Defaults to rawdata.path
#' @export
#' @importFrom git2r repository add
write_delim_git <- function(x, file, path, repo.path = rawdata.path){
  if(class(x) != "data.frame"){
    stop("x is not a data.frame")
  }
  if(!is_git_repo(path = repo.path)){
    stop(repo.path, " is not a git repository")
  }
  filename <- paste(repo.path, path, file, sep = "/")
  write.table(
    x = x, file = filename, append = FALSE, 
    quote = FALSE, sep = "\t", row.names = FALSE, fileEncoding = "UTF-8"
  )
  repo <- repository(repo.path, discover = FALSE)
  add(repo, filename)
  return(invisible(TRUE))
}
