#' Open a git connection
#' @name git_connection
#' @rdname gitConnection-class
#' @param repo.path The path of the root of the repository
#' @param local.path A path within the repository
#' @param key Optional: the path to a private ssh key. The public key is assumed to have the same path with a '.pub' extension. Using in case of ssh authentication.
#' @param username The optional username used in case of  https authentication. Ignored when \code{key} is provided. 
#' @param password The password required for the ssh key or the username. Should be missing when the ssh-key doesn't require a password.
#' @export
#' @importFrom methods new
#' @importFrom git2r repository cred_ssh_key cred_user_pass
#' @include gitConnection_class.R
git_connection <- function(repo.path, local.path = ".", key, username, password){
  repo.path <- check_git_repo(path = repo.path)
  if(missing(key) & missing(username)){
    return(
      new(
        "gitConnection",
        Repository = repository(repo.path),
        LocalPath = local.path,
        Credentials = NULL
      )
    )
  }
  if(missing(username)){
    if(missing(password)){
      return(
        new(
          "gitConnection",
          Repository = repository(repo.path),
          LocalPath = local.path,
          Credentials = cred_ssh_key(
            publickey = paste0(key, ".pub"),
            privatekey = check_single_character(key, name = "key")
          )
        )
      )
    }
    return(
      new(
        "gitConnection",
        Repository = repository(repo.path),
        LocalPath = local.path,
        Credentials = cred_ssh_key(
          publickey = paste0(key, ".pub"),
          privatekey = check_single_character(key, name = "key"),
          passphrase = check_single_character(password, name = "password")
        )
      )
    )
  }
  return(
    new(
      "gitConnection",
      Repository = repository(repo.path),
      LocalPath = local.path,
      Credentials = cred_user_pass(
        username = check_single_character(username, name = "username"),
        password = check_single_character(password, name = "password")
      )
    )
  )
}
