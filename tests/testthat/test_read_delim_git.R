context("read data.frame from git")
describe("read_delim_git()", {
  library(git2r)
  file <- "test.txt"
  path <- "test"
  repo.path <- normalizePath(tempfile(pattern="git2r-"), winslash = "/", mustWork = FALSE)
  df <- data.frame(x = 1, y = 1:10)
  
  
  it("stops is repo.path is not a git repository", {
    expect_that(
      read_delim_git(file = file, path = path, repo.path = repo.path),
      throws_error(paste(repo.path, "is not a git repository.*"))
    )
  })

  dir.create(repo.path)
  repo <- init(repo.path)
  it("returns FALSE when the file doesn't exists", {
    expect_that(
      read_delim_git(file = file, path = path, repo.path = repo.path),
      is_false()
    )
    dir.create(paste(repo.path, path, sep = "/"))
    expect_that(
      read_delim_git(file = file, path = path, repo.path = repo.path),
      is_false()
    )
  })
  write_delim_git(x = df, file = file, path = path, repo.path = repo.path)
  it("read the tab-delimited file", {
    expect_that(
      read_delim_git(file = file, path = path, repo.path = repo.path),
      is_equivalent_to(df)
    )
  })
})
