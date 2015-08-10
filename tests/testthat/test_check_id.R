context("check if an id exists")
describe("check_id()", {
  channel <- connect_result()
  table <- "DatasourceType"
  variable <- "ID"
  variable.text <- "Description"
  value.text <- "'git, tab delimited ssh'"
  sql <- paste(
    "SELECT", variable, "FROM", table, "WHERE", variable.text, "=", value.text
  )
  if (class(channel) == "RODBC") {
    value <- RODBC::sqlQuery(channel = channel, query = sql)[, 1]
  } else {
    value <- 1
  }
  junk <- "junk"

  it("tests if the channel in an ODBC connection", {
    expect_that(
      check_id(
        value = value,
        variable = variable,
        table = table,
        channel = junk
      ),
      throws_error("channel is not an ODBC connection")
    )
  })
  it("tests if the table exists in the ODBC connection", {
    skip_on_cran()
    expect_that(
      check_id(
        value = value,
        variable = variable,
        table = junk,
        channel = channel
      ),
      throws_error(paste("Table\\(s\\) missing:", junk))
    )
  })
  it("tests if data type is correct", {
    skip_on_cran()
    expect_that(
      check_id(
        value = 999999999,
        variable = variable.text,
        table = table,
        channel = channel
      ),
      throws_error(".*Conversion failed when converting.*")
    )
  })
  it("tests if the variable table exists in the table", {
    skip_on_cran()
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
  if (class(channel) == "RODBC") {
    RODBC::odbcClose(channel)
  }
})
