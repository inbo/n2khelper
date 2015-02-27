context("test if an id exists")
describe("test_id()", {
  channel <- watervogelanalysis::connect_watervogel()
  table <- "tblSoort"
  field <- "EuringNummer"
  value <- 70
  junk <- "junk"

  it("tests if the channel in an ODBC connection", {
    expect_that(
      test_id(value = value, field = field, table = table, channel = junk),
      throws_error("channel is not an ODBC connection")
    )
  })
  it("tests if the table exists in the ODBC connection", {
    expect_that(
      test_id(value = value, field = field, table = junk, channel = channel),
      throws_error(paste0("The table '", junk, "' doesn't exists in the ODBC data source"))
    )
  })
  it("tests if the field table exists in the table", {
    expect_that(
      test_id(value = value, field = junk, table = table, channel = channel),
      throws_error(paste0("The field '", junk, "' doesn't exists in table '", table, "'"))
    )
  })
  it("tests if the id exists in the table", {
    expect_that(
      test_id(value = 999999999, field = field, table = table, channel = channel),
      is_false()
    )
  })
  RODBC::odbcClose(channel)
})
