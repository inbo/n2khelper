#' @importClassesFrom git2r cred_user_pass cred_ssh_key
#' @importFrom methods setClassUnion
setClassUnion("gitCredentials", c("NULL", "cred_user_pass", "cred_ssh_key"))

#' The gitConnection class
#'
#' @section Slots:
#'   \describe{
#'    \item{\code{Repository}}{a git repository}
#'    \item{\code{LocalPath}}{a subdirectory wihtin the repository}
#'    \item{\code{Credentials}}{the credentials for the repository}
#'    \item{\code{CommitUser}}{the name of the user how will commit}
#'    \item{\code{CommitEmail}}{the email of the user how will commit}
#'   }
#' @name gitConnection-class
#' @rdname gitConnection-class
#' @exportClass gitConnection
#' @aliases gitConnection-class
#' @importFrom methods setClass
#' @importClassesFrom git2r git_repository
#' @docType class
setClass(
  Class = "gitConnection",
  representation = representation(
    Repository = "git_repository",
    LocalPath = "character",
    Credentials = "gitCredentials",
    CommitUser = "character",
    CommitEmail = "character"
  )
)

#' @importFrom methods setValidity
#' @importFrom git2r repository config
#' @importFrom assertthat assert_that is.string has_name
setValidity(
  "gitConnection",
  function(object){
    assert_that(is.string(object@CommitUser))
    assert_that(is.string(object@CommitEmail))
    root <- paste(object@Repository@path, ".", sep = "/")
    root <- check_path(path = root, type = "directory")
    full.path <- paste(root, object@LocalPath, sep = "/")
    full.path <- check_path(full.path, type = "directory")
    if (length(grep(root, full.path)) == 0) {
      return(
        paste0(
          "Wrong local path. '", full.path,
          "' is not a subdirectory of '", root, "'"
        )
      )
    }
    repo <- repository(root)
    repo.config <- config(repo)
    assert_that(has_name(repo.config$local, "user.name"))
    assert_that(has_name(repo.config$local, "user.email"))
    assert_that(repo.config$local$user.name == object@CommitUser)
    assert_that(repo.config$local$user.email == object@CommitEmail)
    return(TRUE)
  }
)
