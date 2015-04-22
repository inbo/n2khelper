#' List the files in a path of a git repository
#' @inheritParams write_delim_git
#' @inheritParams base::list.files
#' @name list_files_git
#' @rdname list_files_git
#' @exportMethod list_files_git 
#' @docType methods
#' @importFrom methods setGeneric
#' @include git_connection.R
setGeneric(
  name = "list_files_git", 
  def = function(connection, path, pattern = NULL, full.names = FALSE){
    standard.generic(list_files_git)
  }
)

#' @rdname list_files_git
#' @aliases list_files_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "list_files_git", 
  signature = "ANY", 
  definition = function(connection, path, pattern, full.names){
    this.connection <- git_connection(repo.path = connection, local.path = path)
    list_files_git(connection = this.connection, pattern = pattern, full.names = full.names)
  }
)

#' @rdname list_files_git
#' @aliases list_files_git,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "list_files_git", 
  signature = signature(connection = "git_connection"), 
  definition = function(connection, path, pattern = NULL, full.names = FALSE){
    if(!is.null(pattern)){
      pattern <- check_single_character(pattern, name = "pattern")
    }
    full.names <- check_single_logical(full.names, name = "full.names")
    
    full.path <- paste(connection@Repository@path, connection@LocalPath, sep = "/")
    list.files(path = full.path, pattern = pattern, full.names = full.names)
  }
)
