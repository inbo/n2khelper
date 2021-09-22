#' Open a git connection
#' @name git_connection
#' @rdname gitConnection-class
#' @param repo_path The path of the root of the repository
#' @param key Optional: the path to a private ssh key.
#' The public key is assumed to have the same path with a '.pub' extension.
#' Using in case of ssh authentication.
#' @param username The optional username used in case of  https authentication.
#'    Ignored when `key` is provided.
#' @param password The password required for the ssh key or the username.
#' Should be missing when the ssh-key doesn't require a password.
#' @param commit_user the name of the user how will commit
#' @param commit_email the email of the user how will commit
#' @export
#' @importFrom methods new
#' @importFrom assertthat assert_that is.string
#' @importFrom git2r in_repository repository config cred_ssh_key cred_user_pass
#' @include gitConnection_class.R
git_connection <- function(
  repo_path, key, username, password, commit_user, commit_email
) {
  assert_that(is.string(commit_user))
  assert_that(is.string(commit_email))
  assert_that(
    suppressWarnings(in_repository(path = repo_path)),
    msg = "repo_path is not a git repository"
  )
  repo <- repository(repo_path)
  config(repo, user.name = commit_user, user.email = commit_email)

  if (missing(key) & missing(username)) {
    return(
      new(
        "gitConnection",
        Repository = repo,
        Credentials = NULL,
        CommitUser = commit_user,
        CommitEmail = commit_email
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
          CommitUser = commit_user,
          CommitEmail = commit_email
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
        CommitUser = commit_user,
        CommitEmail = commit_email
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
      CommitUser = commit_user,
      CommitEmail = commit_email
    )
  )
}
