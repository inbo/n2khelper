context("is_chartor")
test_that("is_chartor() works as expected", {
  expect_true(is_chartor(letters))
  expect_true(is_chartor(factor(letters)))
  expect_false(is_chartor(1:10))
  expect_true(is_chartor(factor(1:10)))
  expect_false(is_chartor(data.frame()))
})
test_that("is_chartor() uses a custom error message with assert_that", {
  expect_true(assert_that(is_chartor(letters)))
  expect_true(assert_that(is_chartor(factor(letters))))
  expect_error(
    assert_that(is_chartor(1)),
    "1 is neither character nor factor"
  )
  expect_true(assert_that(is_chartor(factor(1:10))))
  expect_error(
    assert_that(is_chartor(data.frame())),
    "data.frame\\(\\) is neither character nor factor"
  )
})
