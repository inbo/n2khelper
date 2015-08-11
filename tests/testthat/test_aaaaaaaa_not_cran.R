context("test NOT_CRAN environment")
expect_false(is.na(Sys.getenv("NOT_CRAN", unset = NA)))
expect_identical(
  Sys.getenv("NOT_CRAN", unset = NA),
  "false"
)
