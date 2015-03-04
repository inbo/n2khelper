#' Commit staged changes in a git repository with automated message
#' 
#' The mesagge is based on the information returned by \code{\link[utils]{sessionInfo}}
#' @param package The name of the package from which we autocommit
#' @param repo.path The path of the repository. Default to \code{rawdata.path}
#' @param username the username for the git repository
#' @param password the user's password for the git repository
#' @export
#' @importFrom git2r repository commit cred_user_pass head push
auto_commit <- function(package, repo.path = rawdata.path, username, password){
  package <- check_single_character(package)
  
  # define the repository
  if(!is_git_repo(path = repo.path)){
    stop(repo.path, " is not a git repository")
  }
  repo <- repository(repo.path, discover = FALSE)
  
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
    commit(repo = repo, message = message),
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
  if(missing(username) & missing(password)){
    warning("changes committed but not pushed")
    return(invisible(TRUE))
  }
  if(missing(username)){
    stop("No username provided. Changes committed but not pushed.")
  }
  if(missing(password)){
    stop("No password provided. Changes committed but not pushed.")
  }
  credentials <- cred_user_pass(username = username, password = password)
  push(head(repo), credentials = credentials)
  
  return(invisible(TRUE))
}
