context("check if an id exists")
describe("check_id()", {
  table <- "datasource_type"
  variable <- "id"
  variable.text <- "description"
  value.text <- "'git, tab delimited ssh'"
  value <- 1
  sql <- paste(
    "SELECT", variable, "FROM", table, "WHERE", variable.text, "=", value.text
  )
  junk <- "junk"

  it("tests if the channel in an ODBC connection", {
    skip_on_cran()
    expect_that(
      check_id(
        value = value,
        variable = variable,
        table = table,
        channel = junk
      ),
      throws_error("channel does not inherit from class DBIConnection")
    )
  })
  it("tests if the table exists in the ODBC connection", {
    skip_on_cran()
    channel <- connect_result(
      username = Sys.getenv("N2KRESULT_USERNAME"),
      password = Sys.getenv("N2KRESULT_PASSWORD")
    )
    expect_that(
      check_id(
        value = value,
        variable = variable,
        table = junk,
        channel = channel
      ),
      throws_error(
        sprintf(
          "Table\\(s\\) %s not found in schema public on database n2kresult",
          junk
        )
      )
    )
  })
  it("tests if data type is correct", {
    skip_on_cran()
    channel <- connect_result(
      username = Sys.getenv("N2KRESULT_USERNAME"),
      password = Sys.getenv("N2KRESULT_PASSWORD")
    )
    expect_error(
      check_id(
        value = 999999999,
        variable = variable.text,
        table = table,
        channel = channel
      )
    )
  })
  it("tests if the variable table exists in the table", {
    skip_on_cran()
    channel <- connect_result(
      username = Sys.getenv("N2KRESULT_USERNAME"),
      password = Sys.getenv("N2KRESULT_PASSWORD")
    )
    value <- 1
    expect_that(
      check_id(
        value = value,
        variable = junk,
        table = table,
        channel = channel
      ),
      throws_error(paste0("Variable\\(s\\) missing from '", table, "': ", junk))
    )
  })
  it("tests if the id exists in the table", {
    skip_on_cran()
    channel <- connect_result(
      username = Sys.getenv("N2KRESULT_USERNAME"),
      password = Sys.getenv("N2KRESULT_PASSWORD")
    )
    expect_that(
      check_id(
        value = 999999999,
        variable = variable,
        table = table,
        channel = channel
      ),
      is_false()
    )
  })
})
