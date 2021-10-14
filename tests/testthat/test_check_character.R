context("check a character")
describe("check_character()", {
  x <- letters
  x_factor <- factor(x)
  x_na <- c("a", NA)
  x_na_factor <- factor(x_na)
  name <- "name"
  na <- na.fail

  it("checks if the input is character", {
    expect_that(
      check_character(x = 1, name = name, na_action = na),
      throws_error(paste(name, "must be character"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_character(x = 1, na_action = na),
      throws_error(paste("x must be character"))
    )
  })
  it("handles NA depending on na_action", {
    expect_that(
      check_character(x = x_na, name = name),
      throws_error(
        "missing values in object"
      )
    )
    expect_that(
      check_character(x = x_na, name = name, na_action = na.fail),
      throws_error(
        "missing values in object"
      )
    )
    expect_that(
      check_character(x = x_na, name = name, na_action = na.pass),
      is_identical_to(x_na)
    )
    expect_that(
      check_character(x = x_na, name = name, na_action = na.omit),
      is_equivalent_to(x_na[!is.na(x_na)])
    )
  })
  it("converts the input to character when it is a factor", {
    expect_that(
      check_character(x = x_factor, na_action = na),
      is_identical_to(x)
    )
    expect_that(
      check_character(x = x_na_factor, na_action = na.pass),
      is_identical_to(x_na)
    )
  })
  it("doesn't alter the input when character", {
    expect_that(
      check_character(x = x),
      is_identical_to(x)
    )
  })
})
