context("write data.frame to git")
describe("write_delim_git()", {
  x <- data.frame(0)
  x1 <- data.frame(1)
  file <- "test.txt"
  local.path <- "test/subdir"
  connection <- normalizePath(
    tempfile(pattern = "git2r-"),
    winslash = "/",
    mustWork = FALSE
  )

  it("stops if connection is not a git repository", {
    expect_that(
      write_delim_git(
        x = x,
        file = file,
        local.path = local.path,
        connection = connection
      ),
      throws_error(paste0("'", connection, "' is not a directory"))
    )
  })

  dir.create(connection)
  repo <- git2r::init(connection)
  it("stops if the path doesn't exist", {
    expect_that(
      write_delim_git(
        x = x,
        file = file,
        local.path = local.path,
        connection = connection
      ),
      throws_error(paste0(
        "'",
        paste(
          normalizePath(connection, winslash = "/", mustWork = FALSE),
          local.path,
          sep = "/"
        ),
        "' is not a directory"
      ))
    )
  })
  full.path <- paste(connection, local.path, sep = "/")
  dir.create(full.path, recursive = TRUE)
  it("stops if x is not a data.frame", {
    expect_that(
      write_delim_git(
        x = matrix(0),
        file = file,
        local.path = local.path,
        connection = connection
      ),
      throws_error("x is not a data.frame")
    )
  })

  full.file.path <- paste(connection, local.path, file, sep = "/")
  it("returns the sha1 of the file", {
    expect_that(
      write_delim_git(
        x = x,
        file = file,
        local.path = local.path,
        connection = connection
      ),
      is_identical_to(git2r::hashfile(full.file.path))
    )
  })

  it("stages the file", {
    expect_that(
      git2r::status(repo)$staged$new,
      is_identical_to(paste(local.path, file, sep = "/"))
    )
    junk <- git2r::commit(repo, "a")
    write_delim_git(
      x = x1,
      file = file,
      local.path = local.path,
      connection = connection
    )
    expect_that(
      git2r::status(repo)$staged$modified,
      is_identical_to(paste(local.path, file, sep = "/"))
    )
  })
})
