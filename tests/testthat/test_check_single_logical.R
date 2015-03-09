context("check a single logical")
describe("check_single_logical()", {
  x <- TRUE
  name <- "name"
  
  it("checks if the input is logical", {
    expect_that(
      check_single_logical(x = 1, name = name),
      throws_error(paste(name, "must be logical"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_single_logical(x = 1),
      throws_error(paste("x must be logical"))
    )
  })
  it("does not allow NA", {
    expect_that(
      check_single_logical(x = NA, name = name),
      throws_error(paste(name, "must be TRUE or FALSE"))
    )
  })
  it("checks if the input is a single logical", {
    expect_that(
      check_single_logical(x = logical(0), name = name),
      throws_error(paste(name, "must be a single logical"))
    )
    expect_that(
      check_single_logical(x = logical(2), name = name),
      throws_error(paste(name, "must be a single logical"))
    )
  })
  it("doesn't alter the input when logical", {
    expect_that(
      check_single_logical(x = x),
      is_identical_to(x)
    )
  })
})
