context("check a file or directory")
describe("check_path()", {
  path <- tempfile(pattern = "testpath")
  name <- "junk.txt"
  file.path <- paste(path, name, sep = "/")
  full.path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  full.file.path <- normalizePath(file.path, winslash = "/", mustWork = FALSE)

  it("stops if path doesn't exists", {
    expect_that(
      check_path(path = path, type = "directory", error = TRUE),
      throws_error(paste0("'", full.path, "' is not a directory"))
    )
    expect_that(
      check_path(path = path, type = "file", error = TRUE),
      throws_error(paste0("'", full.path, "' is not a file"))
    )
    expect_that(
      check_path(path = file.path, type = "directory", error = TRUE),
      throws_error(paste0("'", full.file.path, "' is not a directory"))
    )
    expect_that(
      check_path(path = file.path, type = "file", error = TRUE),
      throws_error(paste0("'", full.file.path, "' is not a file"))
    )
    expect_that(
      check_path(path = path, type = "directory", error = FALSE),
      is_false()
    )
    expect_that(
      check_path(path = path, type = "file", error = FALSE),
      is_false()
    )
    expect_that(
      check_path(path = file.path, type = "directory", error = FALSE),
      is_false()
    )
    expect_that(
      check_path(path = file.path, type = "file", error = FALSE),
      is_false()
    )
  })
  dir.create(full.path)
  content <- paste(sample(letters, 8, replace = TRUE), collapse = "")
  writeLines(content, con = full.file.path)

  it("stops when path exists but is not the correct type", {
    expect_that(
      check_path(path = file.path, type = "directory", error = TRUE),
      throws_error(
        paste0(
          "'",
          normalizePath(full.file.path, winslash = "/"),
          "' is not a directory"
        )
      )
    )
    expect_that(
      check_path(path = path, type = "file", error = TRUE),
      throws_error(
        paste0("'", normalizePath(full.path, winslash = "/"), "' is not a file")
      )
    )
    expect_that(
      check_path(path = file.path, type = "directory", error = FALSE),
      is_false()
    )
    expect_that(
      check_path(path = path, type = "file", error = FALSE),
      is_false()
    )
  })
  it("returns the normalised path", {
    expect_that(
      check_path(path = path, type = "directory", error = TRUE),
      is_identical_to(normalizePath(full.path, winslash = "/"))
    )
    expect_that(
      check_path(path = file.path, type = "file", error = TRUE),
      is_identical_to(normalizePath(full.file.path, winslash = "/"))
    )
  })
})
