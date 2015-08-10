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
  def = function(file, connection, ...){
    standard.generic("read_delim_git")
  }
)

#' @rdname read_delim_git
#' @aliases read_delim_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "read_delim_git", 
  signature = signature(connection = "ANY"),
  definition = function(file, connection, ...){
    this.connection <- git_connection(repo.path = connection, ...)
    read_delim_git(file = file, connection = this.connection)
  }
)

#' @rdname read_delim_git
#' @aliases read_delim_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "read_delim_git",
  signature = signature(connection = "gitConnection"),
  definition = function(file, connection, ...){
    file <- check_single_character(x = file, name = "file")
    
    filename <- paste(connection@Repository@path, connection@LocalPath, file, sep = "/")
    filename <- check_path(path = filename, type = "file", error = TRUE)
    return(read.delim(filename, stringsAsFactors = FALSE))
  }
)
