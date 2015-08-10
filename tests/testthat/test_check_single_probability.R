context("check a single probability")
describe("check_single_probability()", {
  x <- 0.5
  name <- "name"

  it("checks if the input is probability", {
    expect_that(
      check_single_probability(x = "a", name = name),
      throws_error(paste(name, "must be numeric"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_single_probability(x = "a"),
      throws_error(paste("x must be numeric"))
    )
  })
  it("does not allow NA", {
    expect_that(
      check_single_probability(x = NA, name = name),
      throws_error(paste(name, "must be numeric"))
    )
  })
  it("checks if the input is a single probability", {
    expect_that(
      check_single_probability(x = numeric(0), name = name),
      throws_error(paste(name, "must be a single numeric"))
    )
    expect_that(
      check_single_probability(x = numeric(2), name = name),
      throws_error(paste(name, "must be a single numeric"))
    )
  })
  it("checks if the input is with the 0:1 range", {
    expect_that(
      check_single_probability(x = - .Machine$double.eps, name = name),
      throws_error(paste(name, "must be positive"))
    )
    expect_that(
      check_single_probability(x = 1 + .Machine$double.eps, name = name),
      throws_error(paste(name, "must be smaller than 1"))
    )
    expect_that(
      check_single_probability(x = 0, name = name),
      is_identical_to(0)
    )
    expect_that(
      check_single_probability(x = 1, name = name),
      is_identical_to(1)
    )
  })
  it("doesn't alter the input when probability", {
    expect_that(
      check_single_probability(x = x),
      is_identical_to(x)
    )
  })
})
