#' Open a git connection
#' @name git_connection
#' @rdname gitConnection-class
#' @param repo.path The path of the root of the repository
#' @param key Optional: the path to a private ssh key. The public key is assumed
#'    to have the same path with a '.pub' extension. Using in case of ssh
#'    authentication.
#' @param username The optional username used in case of  https authentication.
#'    Ignored when \code{key} is provided.
#' @param password The password required for the ssh key or the username. Should
#'    be missing when the ssh-key doesn't require a password.
#' @param commit.user the name of the user how will commit
#' @param commit.email the email of the user how will commit
#' @export
#' @importFrom methods new
#' @importFrom assertthat assert_that is.string
#' @importFrom git2r in_repository repository config cred_ssh_key cred_user_pass
#' @include gitConnection_class.R
git_connection <- function(
  repo.path,
  key,
  username,
  password,
  commit.user,
  commit.email
){
  assert_that(is.string(commit.user))
  assert_that(is.string(commit.email))
  assert_that(
    suppressWarnings(in_repository(path = repo.path)),
    msg = "repo.path is not a git repository"
  )
  repo <- repository(repo.path)
  config(repo, user.name = commit.user, user.email = commit.email)

  if (missing(key) & missing(username)) {
    return(
      new(
        "gitConnection",
        Repository = repo,
        Credentials = NULL,
        CommitUser = commit.user,
        CommitEmail = commit.email
      )
    )
  }

  if (missing(username)) {
    assert_that(is.string(key))

    if (missing(password)) {
      return(
        new(
          "gitConnection",
          Repository = repo,
          Credentials = cred_ssh_key(
            publickey = paste0(key, ".pub"),
            privatekey = key
          ),
          CommitUser = commit.user,
          CommitEmail = commit.email
        )
      )
    }

    assert_that(is.string(password))
    return(
      new(
        "gitConnection",
        Repository = repo,
        Credentials = cred_ssh_key(
          publickey = paste0(key, ".pub"),
          privatekey = key,
          passphrase = password
        ),
        CommitUser = commit.user,
        CommitEmail = commit.email
      )
    )
  }

  assert_that(is.string(username))
  assert_that(username != "")
  assert_that(is.string(password))
  assert_that(password != "")
  return(
    new(
      "gitConnection",
      Repository = repo,
      Credentials = cred_user_pass(
        username = username,
        password = password
      ),
      CommitUser = commit.user,
      CommitEmail = commit.email
    )
  )
}
