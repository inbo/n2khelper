test_that("check_single_posix()", {
  old_ct <- as.POSIXct("1900-01-01 01:02:03")
  old_lt <- as.POSIXlt("1900-01-01 01:02:03")
  future <- as.POSIXct("2100-01-01 01:02:03")
  name <- "junk"

  # checks if the input is POSIX
  expect_that(
    check_single_posix(x = name, name = name),
    throws_error(paste(name, "must be a POSIXct or POSIXlt object"))
  )
  # uses 'x' as default name
  expect_that(
    check_single_posix(x = 1),
    throws_error(paste("x must be a POSIXct or POSIXlt object"))
  )
  # checks if the length is 1
  expect_that(
    check_single_posix(x = c(old_ct, old_lt), name = name),
    throws_error(paste(name, "must be a single POSIXct or POSIXlt object"))
  )

  # doesn't alter the input when single POSIX
  expect_that(
    check_single_posix(x = old_ct),
    is_identical_to(old_ct)
  )
  expect_that(
    check_single_posix(x = old_lt),
    is_identical_to(old_lt)
  )

  #checks is the date is from the past, but not by default
  expect_that(
    check_single_posix(x = future),
    is_identical_to(future)
  )
  expect_that(
    check_single_posix(x = future, past = FALSE),
    is_identical_to(future)
  )
  expect_that(
    check_single_posix(x = future, past = TRUE, name = name),
    throws_error(paste(name, "is in the future."))
  )
})
