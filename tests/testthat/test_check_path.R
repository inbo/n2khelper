context("check a file or directory")
describe("check_path()", {
  path <- tempfile(pattern = "testpath")
  name <- "junk.txt"
  file_path <- paste(path, name, sep = "/")
  full_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  full_file_path <- normalizePath(file_path, winslash = "/", mustWork = FALSE)

  it("stops if path doesn't exists", {
    expect_that(
      check_path(path = path, type = "directory", error = TRUE),
      throws_error(paste0("'", full_path, "' is not a directory"))
    )
    expect_that(
      check_path(path = path, type = "file", error = TRUE),
      throws_error(paste0("'", full_path, "' is not a file"))
    )
    expect_that(
      check_path(path = file_path, type = "directory", error = TRUE),
      throws_error(paste0("'", full_file_path, "' is not a directory"))
    )
    expect_that(
      check_path(path = file_path, type = "file", error = TRUE),
      throws_error(paste0("'", full_file_path, "' is not a file"))
    )
    expect_false(
      check_path(path = path, type = "directory", error = FALSE)
    )
    expect_false(
      check_path(path = path, type = "file", error = FALSE)
    )
    expect_false(
      check_path(path = file_path, type = "directory", error = FALSE)
    )
    expect_false(
      check_path(path = file_path, type = "file", error = FALSE)
    )
  })
  dir.create(full_path)
  content <- paste(sample(letters, 8, replace = TRUE), collapse = "")
  writeLines(content, con = full_file_path)

  it("stops when path exists but is not the correct type", {
    expect_that(
      check_path(path = file_path, type = "directory", error = TRUE),
      throws_error(
        paste0(
          "'",
          normalizePath(full_file_path, winslash = "/"),
          "' is not a directory"
        )
      )
    )
    expect_that(
      check_path(path = path, type = "file", error = TRUE),
      throws_error(
        paste0("'", normalizePath(full_path, winslash = "/"), "' is not a file")
      )
    )
    expect_false(
      check_path(path = file_path, type = "directory", error = FALSE)
    )
    expect_false(
      check_path(path = path, type = "file", error = FALSE)
    )
  })
  it("returns the normalised path", {
    expect_that(
      check_path(path = path, type = "directory", error = TRUE),
      is_identical_to(normalizePath(full_path, winslash = "/"))
    )
    expect_that(
      check_path(path = file_path, type = "file", error = TRUE),
      is_identical_to(normalizePath(full_file_path, winslash = "/"))
    )
  })
})
