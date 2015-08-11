context("check a single strictly positive integer")
describe("check_single_strictly_positive_integer()", {
  name <- "name"
  tolerance <- 1e-10

  it("checks if the input is integer", {
    expect_that(
      check_single_strictly_positive_integer(
        x = "a",
        name = name,
        tolerance = tolerance
      ),
      throws_error(paste(name, "must be integer"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_single_strictly_positive_integer(x = "a"),
      throws_error(paste("x must be integer"))
    )
  })
  it("checks if the input is a single number", {
    expect_that(
      check_single_strictly_positive_integer(x = integer(0), name = name),
      throws_error(paste(name, "must be a single number"))
    )
    expect_that(
      check_single_strictly_positive_integer(x = integer(2), name = name),
      throws_error(paste(name, "must be a single number"))
    )
  })
  it("checks if the input is strictly positive", {
    expect_that(
      check_single_strictly_positive_integer(x = -1, name = name),
      throws_error(paste(name, "must be strictly positive"))
    )
    expect_that(
      check_single_strictly_positive_integer(x = 0, name = name),
      throws_error(paste(name, "must be strictly positive"))
    )
  })
  it("checks if the input can be rounded to an integer when it is numeric", {
    expect_that(
      check_single_strictly_positive_integer(
        x = 1e-6,
        name = name,
        tolerance = 1e-5
      ),
      throws_error(paste(name, "must be strictly positive"))
    )
    expect_that(
      check_single_strictly_positive_integer(
        x = 1e-6,
        name = name,
        tolerance = 1e-7
      ),
      throws_error(paste(name, "is not integer"))
    )
    expect_that(
      check_single_strictly_positive_integer(
        x = 1 + 1e-6,
        name = name,
        tolerance = 1e-5
      ),
      is_identical_to(1L)
    )
  })
  it("convertes the input to integer when numeric", {
    expect_that(
      check_single_strictly_positive_integer(x = 2),
      is_identical_to(2L)
    )
  })
  it("doesn't alter the input when integer", {
    expect_that(
      check_single_strictly_positive_integer(x = 2L),
      is_identical_to(2L)
    )
  })
})
