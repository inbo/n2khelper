#' Commit staged changes in a git repository with automated message
#' 
#' The mesagge is based on the information returned by \code{\link[utils]{sessionInfo}}
#' @param package The name of the package from which we autocommit
#' @param connection The path of the repository. Default to \code{rawdata.path}
#' @param username the username for the git repository
#' @param password the user's password for the git repository
#' @name auto_commit
#' @rdname auto_commit
#' @exportMethod auto_commit 
#' @docType methods
#' @importFrom methods setGeneric
#' @include git_connection.R
setGeneric(
  name = "auto_commit", 
  def = function(
    package, 
    connection, 
    username = character(0), 
    password = character(0)
  ){
    standard.generic(autocommit)
  }
)

#' @rdname auto_commit
#' @aliases auto_commit,git_connection-methods
#' @importFrom methods setMethod
#' @importFrom git2r repository commit cred_user_pass head push
setMethod(
  f = "auto_commit", 
  signature = "ANY", 
  definition = function(
    package, 
    connection, 
    username = character(0), 
    password = character(0)
  ){
    this.connection <- git_connection(
      repo.path = connection,
      username = username,
      password = password
    )
    auto_commit(
      package = package, 
      connection = this.connection
    )
  }
)


#' @rdname auto_commit
#' @aliases auto_commit,git_connection-methods
#' @importFrom methods setMethod
#' @importFrom git2r commit cred_user_pass head push
setMethod(
  f = "auto_commit",
  signature = signature(connection = "git_connection"),
  definition = function(
    package, 
    connection, 
    username = character(0), 
    password = character(0)
  ){
    package <- check_single_character(package)
    
    #format commit message based on sessionInfo()
    info <- sessionInfo()
    format.other <- function(x){
      paste0(x$Package, " ", x$Version, " built ", x$Built, "\n")
    }
    message <- paste0(
      "Automatic commit from ", package, "\n\n",
      info$R.version$version.string, " revision ", info$R.version$'svn rev', " on ", 
      info$R.version$platform, "\n",
      "\nBase packages: ", paste0(info$basePkgs, collapse = ", "), "\n",
      "\nOther package(s):\n", paste(sapply(info$otherPkgs, format.other), collapse = ""),
      "\nLoaded via a namespace:\n", paste(sapply(info$loadedOnly, format.other), collapse = "")
    )
    
    committed <- tryCatch(
      commit(repo = connection@Repository, message = message),
      error = function(e){
        if(e$message == "Error in 'git2r_commit': Nothing added to commit\n"){
          FALSE
        } else {
          e
        }
      }
    )
    if("error" %in% class(committed)){
      stop(committed)
    }
    if(class(committed) != "git_commit"){
      return(invisible(TRUE))
    }
    if(length(connection@Credential@username) == 0){
      warning("changes committed but not pushed")
    } else {
      push(head(connection@Repository), credentials = connection@Credential)
    }
    return(invisible(TRUE))
  }
)
