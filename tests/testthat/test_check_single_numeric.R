context("check a single numeric")
describe("check_single_numeric()", {
  x <- 0.5
  name <- "name"

  it("checks if the input is numeric", {
    expect_that(
      check_single_numeric(x = "a", name = name),
      throws_error(paste(name, "must be numeric"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_single_numeric(x = "a"),
      throws_error(paste("x must be numeric"))
    )
  })
  it("does not allow NA", {
    expect_that(
      check_single_numeric(x = NA, name = name),
      throws_error(paste(name, "must be numeric"))
    )
  })
  it("checks if the input is a single numeric", {
    expect_that(
      check_single_numeric(x = numeric(0), name = name),
      throws_error(paste(name, "must be a single numeric"))
    )
    expect_that(
      check_single_numeric(x = numeric(2), name = name),
      throws_error(paste(name, "must be a single numeric"))
    )
  })
  it("doesn't alter the input when numeric", {
    expect_that(
      check_single_numeric(x = x),
      is_identical_to(x)
    )
    expect_that(
      check_single_numeric(x = 1L),
      is_identical_to(1L)
    )
  })
})
