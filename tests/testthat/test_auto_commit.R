context("autocommit changes")
describe("auto_commit()", {
  package <- "test"
  user <- "test"
  pwd <- "test"
  
  library(git2r)
  # function to create and stage a file
  dummy_add <- function(repo.path){
    content <- paste(sample(letters, 8, replace = TRUE), collapse = "")
    writeLines(content, paste(repo.path, content, sep = "/"))
    git2r::add(repository(repo.path), content)
  }
  
  # create test repository
  origin.path <- tempfile(pattern="git2r-")
  repo.path <- tempfile(pattern="git2r-")
  dir.create(origin.path)
  dir.create(repo.path)
  repo_bare <- git2r::init(origin.path, bare = TRUE)
  repo <- git2r::clone(origin.path, repo.path)
  dummy_add(repo.path)
  git2r::commit(repo, "inital")
  git2r::push(repo, "origin", "refs/heads/master")
  
  it("gives a warning when no username and password are provided and returns TRUE", {
    dummy_add(repo.path = repo.path)
    expect_that(
      auto_commit(package = package, repo.path = repo.path),
      gives_warning("changes committed but not pushed")
    )
    dummy_add(repo.path = repo.path)
    expect_that(
      auto_commit(package = package, repo.path = repo.path),
      is_true()
    )
  })
  it("returns TRUE when nothing to commit", {
    expect_that(
      auto_commit(package = package, repo.path = repo.path),
      is_true()
    )
  })
  it("returns TRUE when nothing to commit", {
    dummy_add(repo.path = repo.path)
    expect_that(
      auto_commit(package = package, repo.path = repo.path, username = user),
      throws_error("No password provided. Changes committed but not pushed.*")
    )
    dummy_add(repo.path = repo.path)
    expect_that(
      auto_commit(package = package, repo.path = repo.path, password = pwd),
      throws_error("No username provided. Changes committed but not pushed.*")
    )
  })
  it("returns TRUE when changes are pushed", {
    dummy_add(repo.path = repo.path)
    expect_that(
      auto_commit(package = package, repo.path = repo.path, username = user, password = pwd),
      is_true()
    )
  })
  it("writes a correct message title", {
    dummy_add(repo.path = repo.path)
    auto_commit(package = package, repo.path = repo.path, username = user, password = pwd)
    expect_that(
      reflog(repo)[[1]]@message,
      is_identical_to(paste("commit: Automatic commit from", package))
    )
  })
})
