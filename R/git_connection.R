#' The git_connection class
#' 
#' @section Slots:
#'   \describe{
#'    \item{\code{Repository}}{a git repository}
#'    \item{\code{LocalPath}}{a subdirectory wihtin the repository}
#'    \item{\code{Credential}}{the credentials for the repository}
#'   }
#' @name git_connection-class
#' @rdname git_connection-class
#' @exportClass git_connection
#' @aliases git_connection-class
#' @importFrom methods setClass
#' @importClassesFrom git2r git_repository
#' @docType class
setClass(
  Class = "git_connection",
  representation = representation(
    Repository = "git_repository",
    LocalPath = "character",
    Credential = "cred_user_pass"
  )
)

setValidity(
  "git_connection",
  function(object){
    root <- paste(object@Repository@path, ".", sep = "/")
    root <- check_path(path = root, type = "directory")
    full.path <- paste(root, object@LocalPath, sep = "/")
    full.path <- check_path(full.path, type = "directory")
    if(length(grep(root, full.path)) == 0){
      return(paste0("Wrong local path. '", full.path, "' is not a subdirectory of '", root, "'"))
    }
    return(TRUE)
  }
)

#' Open a git connection
#' @name git_connection
#' @rdname git_connection-class
#' @param repo.path The path of the root of the repository
#' @param local.path A path within the repository
#' @inheritParams odbc_connect
#' @export
#' @importFrom methods new
#' @importFrom git2r repository cred_user_pass
git_connection <- function(
  repo.path, 
  local.path = ".", 
  username = character(0), 
  password = character(0)
){
  repo.path <- check_git_repo(path = repo.path)
  if(length(username) != length(password)){
    stop("Provide either both username and password or neither.")
  }
  if(length(username) > 1){
    stop("Multiple usernames are not accepted")
  }
  new(
    "git_connection",
    Repository = repository(repo.path),
    LocalPath = local.path,
    Credential = cred_user_pass(username = username, password = password)
  )
}
