context("check a single character")
describe("check_single_character()", {
  x <- "test"
  name <- "name"
  
  it("checks if the input is character", {
    expect_that(
      check_single_character(x = 1, name = name),
      throws_error(paste(name, "must be character"))
    )
  })
  it("uses 'x' as default name", {
    expect_that(
      check_single_character(x = 1),
      throws_error(paste("x must be character"))
    )
  })
  it("does not allow NA", {
    expect_that(
      check_single_character(x = NA, name = name),
      throws_error(paste(name, "must be character"))
    )
  })
  it("checks if the input is a single character", {
    expect_that(
      check_single_character(x = character(0), name = name),
      throws_error(paste(name, "must be a single character"))
    )
    expect_that(
      check_single_character(x = character(2), name = name),
      throws_error(paste(name, "must be a single character"))
    )
  })
  it("converts the input the character when it is a factor", {
    one.level <- factor(x)
    two.level <- factor(x, levels = paste0(c("a", ""), x))
    expect_that(
      check_single_character(x = two.level),
      is_identical_to(x)
    )
    expect_that(
      check_single_character(x = two.level),
      is_identical_to(x)
    )
  })
  it("doesn't alter the input when character", {
    expect_that(
      check_single_character(x = x),
      is_identical_to(x)
    )
  })
})
