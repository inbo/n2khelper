context("check if an id exists")
describe("check_id()", {
  table <- "datasource_type"
  variable <- "id"
  variable_text <- "description"
  value_text <- "'git, tab delimited ssh'"
  value <- 1
  sql <- paste(
    "SELECT", variable, "FROM", table, "WHERE", variable_text, "=", value_text
  )
  junk <- "junk"

  it("tests if the channel in an ODBC connection", {
    skip_on_cran()
    expect_error(
      check_id(
        value = value,
        variable = variable,
        table = table,
        channel = junk
      ),
      "channel does not inherit from class DBIConnection"
    )
  })
  it("tests if the table exists in the ODBC connection", {
    skip_on_cran()
    channel <- connect_ut_db()
    expect_error(
      check_id(
        value = value,
        variable = variable,
        table = junk,
        channel = channel
      ),
      sprintf(
        "Table\\(s\\) %s not found in schema public on database",
        junk
      )
    )
    DBI::dbDisconnect(channel)
  })
  it("tests if the variable table exists in the table", {
    skip_on_cran()
    channel <- connect_ut_db()
    value <- 1
    expect_error(
      check_id(
        value = value,
        variable = junk,
        table = table,
        channel = channel
      ),
      paste0("Variable\\(s\\) missing from '", table, "': ", junk)
    )
    DBI::dbDisconnect(channel)
  })
  it("tests if the id exists in the table", {
    skip_on_cran()
    channel <- connect_ut_db()
    expect_false(
      check_id(
        value = 99999999,
        variable = variable,
        table = table,
        channel = channel
      )
    )
    DBI::dbDisconnect(channel)
  })
})
