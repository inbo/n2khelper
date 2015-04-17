context("list files for a git repository")
describe("list_files_git()", {
  files <- sort(c("test.txt", "0123456.txt", "test", "0123456"))
  path <- "test"
  repo.path <- tempfile(pattern="git2r-")
  repo.path <- normalizePath(repo.path, winslash = "/", mustWork = FALSE)
  
  
  it("stops is repo.path is not a git repository", {
    expect_that(
      list_files_git(path = path, repo.path = repo.path),
      throws_error(paste(repo.path, "is not a git repository.*"))
    )
  })

  dir.create(repo.path)
  repo <- git2r::init(repo.path)
  repo.path <- normalizePath(repo.path, winslash = "/", mustWork = FALSE)
  full.path <- paste(repo.path, path, sep = "/")
  full.path <- normalizePath(full.path, winslash = "/", mustWork = FALSE)
  it("stops is the path doesn't exist", {
    expect_that(
      list_files_git(path = path, repo.path = repo.path),
      throws_error(paste(full.path, "is not a directory.*"))
    )
  })
  
  dir.create(paste(repo.path, path, sep = "/"))
  file.create(paste(full.path, files, sep = "/"))
  it("list the files according to the pattern", {
    expect_that(
      list_files_git(path = path, pattern = "^[0123456789].*\\.txt$", repo.path = repo.path),
      is_identical_to(files[grep("^[0123456789].*\\.txt$", files)])
    )
    expect_that(
      list_files_git(path = path, pattern = "\\.txt$", repo.path = repo.path),
      is_identical_to(files[grep("\\.txt$", files)])
    )
    expect_that(
      list_files_git(path = path, repo.path = repo.path),
      is_identical_to(files)
    )
    expect_that(
      list_files_git(path = path, pattern = ".exe", repo.path = repo.path),
      is_identical_to(character(0))
    )
  })
})
