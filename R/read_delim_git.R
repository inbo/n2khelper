#' Read a tab delimited file from a git repository
#' @inheritParams write_delim_git
#' @name read_delim_git
#' @rdname read_delim_git
#' @exportMethod read_delim_git 
#' @docType methods
#' @importFrom methods setGeneric
#' @include git_connection.R
setGeneric(
  name = "read_delim_git", 
  def = function(file, connection, path){
    standard.generic(read_delim_git)
  }
)

#' @rdname read_delim_git
#' @aliases read_delim_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "read_delim_git", 
  signature = "ANY", 
  definition = function(file, connection, path){
    this.connection <- git_connection(repo.path = connection, local.path = path)
    read_delim_git(file = file, connection = this.connection)
  }
)

#' @rdname read_delim_git
#' @aliases read_delim_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "read_delim_git",
  signature = signature(connection = "git_connection"),
  definition = function(file, connection, path){
    file <- check_single_character(x = file, name = "file")
    
    filename <- paste(connection@Repository@path, connection@LocalPath, file, sep = "/")
    filename <- normalizePath(path = filename, winslash = "/", mustWork = FALSE)
    if(file_test("-f", filename)){
      return(read.delim(filename))
    } else {
      return(FALSE)
    }
  }
)
