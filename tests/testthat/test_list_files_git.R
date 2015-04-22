context("list files for a git repository")
describe("list_files_git()", {
  files <- sort(c("test.txt", "0123456.txt", "test", "0123456"))
  path <- "test"
  connection <- tempfile(pattern="git2r-")
  connection <- normalizePath(connection, winslash = "/", mustWork = FALSE)
  
  
  it("stops is connection is not a git repository", {
    expect_that(
      list_files_git(path = path, connection = connection),
      throws_error(paste0("'", connection, "' is not a directory"))
    )
  })

  dir.create(connection)
  repo <- git2r::init(connection)
  connection <- normalizePath(connection, winslash = "/", mustWork = FALSE)
  full.path <- paste(connection, path, sep = "/")
  full.path <- normalizePath(full.path, winslash = "/", mustWork = FALSE)
  it("stops is the path doesn't exist", {
    expect_that(
      list_files_git(path = path, connection = connection),
      throws_error(paste0("'", connection, "/", path, "' is not a directory"))
    )
  })
  
  dir.create(paste(connection, path, sep = "/"))
  file.create(paste(full.path, files, sep = "/"))
  it("list the files according to the pattern", {
    expect_that(
      list_files_git(path = path, pattern = "^[0123456789].*\\.txt$", connection = connection),
      is_identical_to(files[grep("^[0123456789].*\\.txt$", files)])
    )
    expect_that(
      list_files_git(path = path, pattern = "\\.txt$", connection = connection),
      is_identical_to(files[grep("\\.txt$", files)])
    )
    expect_that(
      list_files_git(path = path, connection = connection),
      is_identical_to(files)
    )
    expect_that(
      list_files_git(path = path, pattern = ".exe", connection = connection),
      is_identical_to(character(0))
    )
  })
})
