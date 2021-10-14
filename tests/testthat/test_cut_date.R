context("convert a date to a period")
describe("cut_date()", {
  x <- as.POSIXct(
    c(
      "2015-01-01 0:0:0", "2014-01-02 0:0:0", "2013-01-03 0:0:0",
      "2012-01-31 23:59:59", "2011-02-01 0:0:0", "2012-12-31 23:59:59.999999"
    )
    )
  dm <- c("1-1", "15-1", "1-2", "31-12")
  dm_format <- c("1/1", "01/01", "40-1", "1 1", "1-1-2015")
  include_last <- TRUE

  it("yields the correct periods", {
    expect_that(
      cut_date(x = x, dm = c("1-1", "31-12"), include_last = FALSE),
      is_identical_to(
        factor(c(rep(1, length(x) - 1), NA), labels = "[1-1, 31-12)")
      )
    )
    expect_that(
      cut_date(x = x, dm = c("1-1", "31-12"), include_last = TRUE),
      is_identical_to(factor(c(rep(1, length(x))), labels = "[1-1, 31-12]"))
    )
    expect_that(
      cut_date(x = x, dm = c("1-1", "1-2", "31-12"), include_last = TRUE),
      is_identical_to(
        factor(
          c(1, 1, 1, 1, 2, 2),
          labels = c("[1-1, 1-2)", "[1-2, 31-12]")
        )
      )
    )
    expect_that(
      cut_date(x = as.Date(x), dm = c("1-1", "31-12"), include_last = TRUE),
      is_identical_to(factor(c(rep(1, length(x))), labels = "[1-1, 31-12]"))
    )
  })
  it("includes the lasted day by default", {
    expect_that(
      cut_date(x = x, dm = c("1-1", "31-12")),
      is_identical_to(factor(c(rep(1, length(x))), labels = "[1-1, 31-12]"))
    )
  })

  it("checks if the input is POSIXt or Date", {
    expect_that(
      cut_date(x = 1, dm = dm),
      throws_error("x does not inherit from class POSIXt")
    )
  })
  it("checks the format of dm", {
    expect_that(
      cut_date(x = x, dm = dm_format),
      throws_error(
        paste(
          "'dm' requires a day-month format. Mismatching values:",
          paste0("'", dm_format, "'", collapse = ", ")
        )
      )
    )
  })
  it("orders dm", {
    expect_that(
      cut_date(x = x, dm = c("31-12", "1-1")),
      is_identical_to(factor(c(rep(1, length(x))), labels = "[1-1, 31-12]"))
    )
  })

})
