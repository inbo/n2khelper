context("check if a database table contains a variable")
describe("check_dbtable_variable()", {
  channel <- connect_result()
  junk <- "junk"
  table <- "Location"
  variable <- c("ExternalCode", "Description")
  error <- TRUE
  
  it("checks if table is a single character", {
    expect_that(
      check_dbtable_variable(table = integer(0), variable = variable, channel = channel, error = error),
      throws_error("table must be character")
    )
    expect_that(
      check_dbtable_variable(table = NA, variable = variable, channel = channel, error = error),
      throws_error("table must be character")
    )
    expect_that(
      check_dbtable_variable(table = character(2), variable = variable, channel = channel, error = error),
      throws_error("table must be a single character")
    )
    expect_that(
      check_dbtable_variable(table = character(0), variable = variable, channel = channel, error = error),
      throws_error("table must be a single character")
    )
  })
  
  it("checks if table exists", {
    skip_on_cran()
    expect_that(
      check_dbtable_variable(table = junk, variable = variable, channel = channel, error = error),
      throws_error(paste("Table\\(s\\) missing:", junk))
    )
  })
  
  it("checks if channel is on ODBC connection", {
    expect_that(
      check_dbtable_variable(table = table, variable = variable, channel = junk, error = error),
      throws_error("channel is not an ODBC connection")
    )
  })
  
  it("checks if variable is a non-empty character without missing values", {
    expect_that(
      check_dbtable_variable(table = table, variable = integer(0), channel = channel, error = error),
      throws_error("variable must be character")
    )
    expect_that(
      check_dbtable_variable(table = table, variable = c(variable, NA), channel = channel, error = error),
      throws_error("Error in na.fail.default\\(x\\) : missing values in object.*")
    )
    expect_that(
      check_dbtable_variable(table = table, variable = character(0), channel = channel, error = error),
      throws_error("'variable' must contain at least one value")
    )
  })
  
  it("gives correct output", {
    skip_on_cran()
    expect_that(
      check_dbtable_variable(table = table, variable = variable, channel = channel, error = error),
      is_true()
    )
    expect_that(
      check_dbtable_variable(table = table, variable = junk, channel = channel, error = FALSE),
      is_false()
    )
    expect_that(
      check_dbtable_variable(table = table, variable = junk, channel = channel, error = FALSE),
      is_false()
    )
    expect_that(
      check_dbtable_variable(table = table, variable = junk, channel = channel, error = TRUE),
      throws_error(paste0("Variable\\(s\\) missing from '", table, "': ", paste(junk, collapse = ", ")))
    )
  })
  if (!identical(Sys.getenv("NOT_CRAN", unset = "false"), "true")) {
    RODBC::odbcClose(channel)
  }
})
