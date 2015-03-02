context("write data.frame to git")
describe("write_delim_git()", {
  file <- "test.txt"
  path <- "test"
  repo.path.junk <- "~"
  
  it("stops if x is not a data.frame", {
    expect_that(
      write_delim_git(x = matrix(0), file = file, path = path),
      throws_error("x is not a data.frame")
    )
  })
  it("stops is repo.path is not a git repository", {
    expect_that(
      write_delim_git(x = data.frame(0), file = file, path = path, repo.path = repo.path.junk),
      throws_error(paste(repo.path.junk, "is not a git repository"))
    )
  })
})
