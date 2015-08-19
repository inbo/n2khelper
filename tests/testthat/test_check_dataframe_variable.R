context("check if a data.frame contains a variable")
describe("check_dataframe_variable()", {
  missing.var <- "aaaa"
  missing.var2 <- c("aaaa", "bbbb")
  df <- data.frame(a = integer(0), b = logical(0), c = character(0))
  variable <- colnames(df)
  variable.subset <- sample(variable, length(variable) - 1)
  name <- "df"
  error <- TRUE
  df.matrix <- as.matrix(df)

  it("checks if df is a data.frame", {
    expect_that(
      check_dataframe_variable(
        df = "",
        variable = variable,
        name = name,
        error = error
      ),
      throws_error(paste(name, "must be a data.frame"))
    )
  })
  it("checks if variable is a non-empty character without missing values", {
    expect_that(
      check_dataframe_variable(
        df = df,
        variable = 10,
        name = name,
        error = error
      ),
      throws_error("variable must be character")
    )
    expect_that(
      check_dataframe_variable(
        df = df,
        variable = c(variable, NA),
        name = name,
        error = error
      ),
      throws_error(
        "Error in na.fail.default\\(x\\) : missing values in object.*"
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df,
        variable = character(0),
        name = name,
        error = error
      ),
      throws_error("'variable' must contain at least one value")
    )
  })

  it("uses 'df' as default name", {
    expect_that(
      check_dataframe_variable(df = "", variable = variable, error = error),
      throws_error("df must be a data.frame")
    )
  })

  it("works on data.frames and matrices", {
    expect_that(
      check_dataframe_variable(
        df = df,
        variable = variable,
        name = name,
        error = error
      ),
      is_true()
    )
    expect_that(
      check_dataframe_variable(
        df = df.matrix,
        variable = variable,
        name = name,
        error = error
      ),
      is_true()
    )
    expect_that(
      check_dataframe_variable(
        df = df,
        variable = variable.subset,
        name = name,
        error = error
      ),
      is_true()
    )
    expect_that(
      check_dataframe_variable(
        df = df.matrix,
        variable = variable.subset,
        name = name,
        error = error
      ),
      is_true()
    )
    expect_that(
      check_dataframe_variable(
        df = df, variable = c(variable, missing.var), name = name, error = TRUE
      ),
      throws_error(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing.var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df.matrix,
        variable = c(variable, missing.var),
        name = name,
        error = TRUE
      ),
      throws_error(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing.var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df, variable = c(variable, missing.var), name = name, error = FALSE
      ),
      gives_warning(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing.var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df.matrix,
        variable = c(variable, missing.var),
        name = name,
        error = FALSE
      ),
      gives_warning(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing.var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df, variable = c(variable, missing.var), name = name, error = FALSE
      ),
      is_false()
    )
    expect_that(
      check_dataframe_variable(
        df = df.matrix,
        variable = c(variable, missing.var),
        name = name,
        error = FALSE
      ),
      is_false()
    )
  })

  it("handles tbl_df from dplyr", {
    expect_true(
      check_dataframe_variable(
        df = dplyr::as.tbl(df),
        variable = variable.subset,
        name = name,
        error = error
      )
    )
  })
})
