context("check a character")
describe("check_character()", {
  x <- letters
  x.factor <- factor(x)
  x.na <- c("a", NA)
  x.na.factor <- factor(x.na)
  name <- "name"
  na <- na.fail

  it("checks if the input is character", {
    expect_that(
      check_character(x = 1, name = name, na.action = na),
      throws_error(paste(name, "must be character"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_character(x = 1, na.action = na),
      throws_error(paste("x must be character"))
    )
  })
  it("handles NA depending on na.action", {
    expect_that(
      check_character(x = x.na, name = name),
      throws_error(
        "missing values in object"
      )
    )
    expect_that(
      check_character(x = x.na, name = name, na.action = na.fail),
      throws_error(
        "missing values in object"
      )
    )
    expect_that(
      check_character(x = x.na, name = name, na.action = na.pass),
      is_identical_to(x.na)
    )
    expect_that(
      check_character(x = x.na, name = name, na.action = na.omit),
      is_equivalent_to(x.na[!is.na(x.na)])
    )
  })
  it("converts the input to character when it is a factor", {
    expect_that(
      check_character(x = x.factor, na.action = na),
      is_identical_to(x)
    )
    expect_that(
      check_character(x = x.na.factor, na.action = na.pass),
      is_identical_to(x.na)
    )
  })
  it("doesn't alter the input when character", {
    expect_that(
      check_character(x = x),
      is_identical_to(x)
    )
  })
})
