context("write data.frame to git")
describe("write_delim_git()", {
  x <- data.frame(0)
  x1 <- data.frame(1)
  file <- "test.txt"
  path <- "test/subdir"
  repo.path <- normalizePath(tempfile(pattern="git2r-"), winslash = "/", mustWork = FALSE)
  
  it("stops if x is not a data.frame", {
    expect_that(
      write_delim_git(x = matrix(0), file = file, path = path, repo.path = repo.path),
      throws_error("x is not a data.frame")
    )
  })
  it("stops is repo.path is not a git repository", {
    expect_that(
      write_delim_git(x = x, file = file, path = path, repo.path = repo.path),
      throws_error(paste(repo.path, "is not a git repository"))
    )
  })
  
  dir.create(repo.path)
  repo <- git2r::init(repo.path)
  it("creates path when it doesn't exists and gives a warning", {
    expect_that(
      write_delim_git(x = x, file = file, path = path, repo.path = repo.path),
      gives_warning(paste(path, "is created"))
    )
    full.path <- paste(repo.path, path, sep = "/")
    expect_that(
      file_test("-d", full.path),
      is_true()
    )
    full.path <- paste(repo.path, path, file, sep = "/")
    expect_that(
      file_test("-f", full.path),
      is_true()
    )
  })
  it("stages the file", {
    expect_that(
      status(repo)$staged$new,
      is_identical_to(paste(path, file, sep = "/"))
    )
    junk <- commit(repo, "a")
    write_delim_git(x = x1, file = file, path = path, repo.path = repo.path)
    expect_that(
      status(repo)$staged$modified,
      is_identical_to(paste(path, file, sep = "/"))
    )
  })
})
