context("read data.frame from git")
describe("read_delim_git()", {
  file <- "test.txt"
  local.path <- "test"
  connection <- normalizePath(
    tempfile(pattern = "git2r-"),
    winslash = "/",
    mustWork = FALSE
  )
  df <- data.frame(x = 1, y = 1:10)


  it("stops is connection is not a git repository", {
    expect_that(
      read_delim_git(
        file = file,
        local.path = local.path,
        connection = connection
      ),
      throws_error(paste0("'", connection, "' is not a directory"))
    )
  })

  dir.create(paste(connection, local.path, sep = "/"), recursive = TRUE)
  repo <- git2r::init(connection)

  it("returns FALSE when the file doesn't exists", {
    expect_that(
      read_delim_git(
        file = file,
        local.path = local.path,
        connection = connection
      ),
      throws_error(
        paste0("'", repo@path, "/", local.path, "/", file, "' is not a file")
      )
    )
  })
  write_delim_git(
    x = df,
    file = file,
    local.path = local.path,
    connection = connection
  )
  it("read the tab-delimited file", {
    expect_that(
      read_delim_git(
        file = file,
        local.path = local.path,
        connection = connection
      ),
      is_equivalent_to(df)
    )
  })
})
