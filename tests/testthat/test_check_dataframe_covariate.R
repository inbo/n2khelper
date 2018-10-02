context("check if a data.frame contains the covariates")
describe("check_dataframe_covariate()", {
  response <- "Count"
  missing.response <- "NoCount"
  covariates.one <- c(
    "A",
    "(1|A)",
    "f(A, model = 'iid')"
  )
  covariates.two <- c(
    "A + B",
    "A : B",
    "A * B",
    "A + (1|B)",
    "1 + f(A, model = 'rw1', replicate = as.integer(B))"
  )
  covariates.three <- c(
    "A + (B|C)",
    "0 + A + (0 + B|C)",
    "A + (A * B|C)"
  )
  df <- data.frame(
    Count = integer(0),
    A = character(0),
    B = character(0),
    C = character(0)
  )

  it("returns TRUE if all variables exist in dataframe", {
    for (covariate in c(covariates.one, covariates.two, covariates.three)) {
      expect_that(
        check_dataframe_covariate(
          df = df, covariate = covariate, response = response, error = TRUE
        ),
        is_true()
      )
    }
  })

  it(
    "returns FALSE with warning if at least one variable doesn't exist in
    dataframe", {
    for (covariate in covariates.one) {
      expect_that(
        suppressWarnings(
          check_dataframe_covariate(
            df = df[, c("Count", "B", "C")],
            covariate = covariate,
            response = response,
            error = FALSE
          )
        ),
        is_false()
      )
      expect_that(
        check_dataframe_covariate(
          df = df[, c("Count", "B", "C")],
          covariate = covariate,
          response = response,
          error = FALSE
        ),
        gives_warning("Variables missing in df: A")
      )
      expect_that(
        suppressWarnings(
          check_dataframe_covariate(
            df = df,
            covariate = covariate,
            response = missing.response,
            error = FALSE
          )
        ),
        is_false()
      )
      expect_that(
        check_dataframe_covariate(
          df = df,
          covariate = covariate,
          response = missing.response,
          error = FALSE
        ),
        gives_warning(paste("Variables missing in df:", missing.response))
      )
    }
    for (covariate in covariates.two) {
      expect_that(
        suppressWarnings(
          check_dataframe_covariate(
            df = df[, c("Count", "C")],
            covariate = covariate,
            response = response,
            error = FALSE
          )
        ),
        is_false()
      )
      expect_that(
        check_dataframe_covariate(
          df = df[, c("Count", "C")],
          covariate = covariate,
          response = response,
          error = FALSE
        ),
        gives_warning("Variables missing in df: A, B")
      )
    }
  })

  it("stops if at least one variable doesn't exist in dataframe", {
    for (covariate in covariates.one) {
      expect_that(
        check_dataframe_covariate(
          df = df[, c("Count", "B", "C")],
          covariate = covariate,
          response = response,
          error = TRUE
        ),
        throws_error("Variables missing in df: A")
      )
      expect_that(
        check_dataframe_covariate(
          df = df,
          covariate = covariate,
          response = missing.response,
          error = TRUE
        ),
        throws_error(paste("Variables missing in df:", missing.response))
      )
    }
    for (covariate in covariates.two) {
      expect_that(
        check_dataframe_covariate(
          df = df[, c("Count", "C")],
          covariate = covariate,
          response = response,
          error = TRUE
        ),
        throws_error("Variables missing in df: A, B")
      )
    }
  })
})
