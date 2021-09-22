context("git_connection")

connection <- tempfile(pattern = "git2r-")
commit_user <- "me"
commit_email <- "me@me.com"

# test repo_path
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "is not a git repository"
)
dir.create(connection)
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "is not a git repository"
)
repo <- git2r::init(connection)

#test commit_user
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = 1,
    commit_email = commit_email
  ),
  "commit_user is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = NA,
    commit_email = commit_email
  ),
  "commit_user is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = rep(commit_user, 2),
    commit_email = commit_email
  ),
  "commit_user is not a string \\(a length one character vector\\)\\."
)

# test commit_email
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = commit_user,
    commit_email = 1
  ),
  "commit_email is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = commit_user,
    commit_email = NA
  ),
  "commit_email is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    commit_user = commit_user,
    commit_email = rep(commit_email, 2)
  ),
  "commit_email is not a string \\(a length one character vector\\)\\."
)

expect_is(
  git_conn <- git_connection(
    repo_path = connection, commit_user = commit_user,
    commit_email = commit_email
  ),
  "gitConnection"
)
expect_identical(
  git2r::config(repo)$local$user.name,
  commit_user
)
expect_identical(
  git2r::config(repo)$local$user.email,
  commit_email
)

expect_is(
  git_connection(
    repo_path = connection,
    username = "me",
    password = "junk",
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "gitConnection"
)

expect_error(
  git_connection(
    repo_path = connection,
    username = rep("me", 2),
    password = "junk",
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "username is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    username = NA,
    password = "junk",
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "username is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    username = "",
    password = "junk",
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "username not not equal to \"\""
)

expect_error(
  git_connection(
    repo_path = connection,
    username = "me",
    password = rep("junk", 2),
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "password is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    username = "me",
    password = NA,
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "password is not a string \\(a length one character vector\\)\\."
)
expect_error(
  git_connection(
    repo_path = connection,
    username = "me",
    password = "",
    commit_user = commit_user,
    commit_email = commit_email
  ),
  "password not not equal to \"\""
)
