context("odbc_insert")
describe(
  "odbc_insert",
  {
    it(
      "tests for infinite values",
      {
        dataset <- data.frame(
          A = letters,
          B = c(rnorm(25), Inf),
          C = seq_along(letters)
        )
        expect_error(
          odbc_insert(
            data = dataset,
            table = "junk",
            channel = "junk"
          ),
          "data contains infinite values"
        )
      }
    )
  }
)
