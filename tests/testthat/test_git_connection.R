context("git_connection")

connection <- tempfile(pattern = "git2r-")
dir.create(connection)
repo <- git2r::init(connection)
expect_is(
  git.connection <- git_connection(repo.path = connection),
  "gitConnection"
)
expect_identical(
  git2r::config(repo)$local$user.name,
  "n2khelper"
)
expect_identical(
  git2r::config(repo)$local$user.email,
  "bmk@inbo.be"
)
