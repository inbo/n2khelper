#' Get the SHA of the files at the HEAD
#' @inheritParams write_delim_git
#' @name git_sha
#' @rdname git_sha
#' @exportMethod git_sha 
#' @docType methods
#' @importFrom methods setGeneric
#' @include git_connection.R
setGeneric(
  name = "git_sha", 
  def = function(file, connection, path){
    standard.generic(git_sha)
  }
)

#' @rdname git_sha
#' @aliases git_sha,git_connection-methods
#' @importFrom methods setMethod
setMethod(
  f = "git_sha", 
  signature = "ANY", 
  definition = function(file, connection, path){
    this.connection <- git_connection(repo.path = connection, local.path = path)
    git_sha(file = file, connection = this.connection)
  } 
)

#' @rdname git_sha
#' @aliases git_sha,git_connection-methods
#' @importFrom methods setMethod
#' @importFrom git2r odb_blobs head branch_target
setMethod(
  f = "git_sha",
  signature = signature(connection = "git_connection"),
  definition = function(file, connection, path){
    file <- check_character(x = file, name = "file")
    
    blobs <- odb_blobs(connection@Repository)
    blobs <- blobs[
      blobs$path == connection@LocalPath & 
      blobs$name %in% file &
      blobs$commit == branch_target(head(connection@Repository))
      , 
    ]
    return(blobs)
  }
)
