context("autocommit changes")
describe("auto_commit()", {
  package <- "test"
  user <- "test"
  pwd <- "test"

  # function to create and stage a file
  dummy_add <- function(connection){
    content <- paste(sample(letters, 8, replace = TRUE), collapse = "")
    writeLines(content, paste(connection, content, sep = "/"))
    git2r::add(git2r::repository(connection), content)
  }

  # create test repository
  origin.path <- tempfile(pattern = "git2r-")
  connection <- tempfile(pattern = "git2rclone-")
  dir.create(origin.path)
  dir.create(connection)
  repo_bare <- git2r::init(origin.path, bare = TRUE)
  repo <- git2r::clone(origin.path, connection)
  git2r::config(repo, user.name = "me", user.email = "me@me.com")
  dummy_add(connection)
  git2r::commit(repo, "inital")
  git2r::push(repo, "origin", "refs/heads/master")

  it("gives a warning when no username and password are provided and returns
     TRUE", {
    dummy_add(connection = connection)
    expect_that(
      auto_commit(package = package, connection = connection),
      gives_warning("changes committed but not pushed")
    )
    dummy_add(connection = connection)
    expect_that(
      auto_commit(package = package, connection = connection),
      is_true()
    )
  })
  it("returns TRUE when nothing to commit", {
    expect_that(
      auto_commit(package = package, connection = connection),
      is_true()
    )
  })
  it("returns TRUE when changes are pushed", {
    dummy_add(connection = connection)
    expect_that(
      auto_commit(
        package = package,
        connection = connection,
        username = user,
        password = pwd
      ),
      is_true()
    )
  })
  it("writes a correct message title", {
    dummy_add(connection = connection)
    auto_commit(
      package = package,
      connection = connection,
      username = user,
      password = pwd
    )
    expect_that(
      git2r::reflog(repo)[[1]]@message,
      is_identical_to(paste("commit: Automatic commit from", package))
    )
  })
})
