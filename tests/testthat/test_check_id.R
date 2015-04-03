context("check if an id exists")
describe("check_id()", {
  channel <- connect_result()
  table <- "DatasourceType"
  field <- "ID"
  field.text <- "Description"
  value.text <- "'git, tab delimited'"
  sql <- paste("SELECT", field, "FROM", table, "WHERE", field.text, "=", value.text)
  value <- RODBC::sqlQuery(channel = channel, query = sql)[, 1]
  junk <- "junk"

  it("tests if the channel in an ODBC connection", {
    expect_that(
      check_id(value = value, field = field, table = table, channel = junk),
      throws_error("channel is not an ODBC connection")
    )
  })
  it("tests if the table exists in the ODBC connection", {
    expect_that(
      check_id(value = value, field = field, table = junk, channel = channel),
      throws_error(paste0("The table '", junk, "' doesn't exists in the ODBC data source"))
    )
  })
  it("tests if data type is correct", {
    expect_that(
      check_id(value = 999999999, field = field.text, table = table, channel = channel),
      throws_error(".*Conversion failed when converting.*")
    )
  })
  it("tests if the field table exists in the table", {
    expect_that(
      check_id(value = value, field = junk, table = table, channel = channel),
      throws_error(paste0("The field '", junk, "' doesn't exists in table '", table, "'"))
    )
  })
  it("tests if the id exists in the table", {
    expect_that(
      check_id(value = 999999999, field = field, table = table, channel = channel),
      is_false()
    )
  })
  RODBC::odbcClose(channel)
})
