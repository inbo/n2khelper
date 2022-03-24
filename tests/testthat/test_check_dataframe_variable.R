context("check if a data.frame contains a variable")
describe("check_dataframe_variable()", {
  missing_var <- "aaaa"
  missing_var2 <- c("aaaa", "bbbb")
  df <- data.frame(a = integer(0), b = logical(0), c = character(0))
  variable <- colnames(df)
  variable_subset <- sample(variable, length(variable) - 1)
  name <- "df"
  error <- TRUE
  df_matrix <- as.matrix(df)

  it("checks if df is a data.frame", {
    expect_error(
      check_dataframe_variable(
        df = "",
        variable = variable,
        name = name,
        error = error
      ),
      "inherits\\(df, \"data\\.frame\\)\" | inherits\\(df, \"matrix\"\\)"
    )
  })
  it("checks if variable is a non-empty character without missing values", {
    expect_error(
      check_dataframe_variable(
        df = df,
        variable = 10,
        name = name,
        error = error
      ),
"Error : is\\.list\\(variable\\) | is\\.character\\(variable\\) is not TRUE"
    )
    expect_error(
      check_dataframe_variable(
        df = df,
        variable = c(variable, NA),
        name = name,
        error = error
      ),
      "variable contains 1 missing values"
    )
    expect_error(
      check_dataframe_variable(
        df = df,
        variable = character(0),
        name = name,
        error = error
      ),
      "length\\(variable\\) not greater than 0" #nolint: nonportable_path_linter, line_length_linter.
    )
  })

  it("works on data.frames and matrices", {
    expect_true(
      check_dataframe_variable(
        df = df,
        variable = variable,
        name = name,
        error = error
      )
    )
    expect_true(
      check_dataframe_variable(
        df = df_matrix,
        variable = variable,
        name = name,
        error = error
      )
    )
    expect_true(
      check_dataframe_variable(
        df = df,
        variable = variable_subset,
        name = name,
        error = error
      )
    )
    expect_true(
      check_dataframe_variable(
        df = df_matrix,
        variable = variable_subset,
        name = name,
        error = error
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df, variable = c(variable, missing_var), name = name, error = TRUE
      ),
      throws_error(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing_var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df_matrix,
        variable = c(variable, missing_var),
        name = name,
        error = TRUE
      ),
      throws_error(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing_var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df, variable = c(variable, missing_var), name = name, error = FALSE
      ),
      gives_warning(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing_var, collapse = ", ")
        )
      )
    )
    expect_that(
      check_dataframe_variable(
        df = df_matrix,
        variable = c(variable, missing_var),
        name = name,
        error = FALSE
      ),
      gives_warning(
        paste0(
          "Variables missing in ", name, ": ",
          paste(missing_var, collapse = ", ")
        )
      )
    )
    expect_false(
      suppressWarnings(
        check_dataframe_variable(
          df = df,
          variable = c(variable, missing_var),
          name = name,
          error = FALSE
        )
      )
    )
    expect_false(
      suppressWarnings(
        check_dataframe_variable(
          df = df_matrix,
          variable = c(variable, missing_var),
          name = name,
          error = FALSE
        )
      )
    )
  })

  it("handles tbl_df from dplyr", {
    expect_true(
      check_dataframe_variable(
        df = tibble::as_tibble(df),
        variable = variable_subset,
        name = name,
        error = error
      )
    )
  })

  it("forces a check on all NA variables", {
    expect_error(
      check_dataframe_variable(
        df = data.frame(A = NA),
        variable = list(A = "character"),
        name = name,
        force_na = TRUE
      ),
      "A: got 'logical', expected 'character'"
    )
    expect_equal(
      check_dataframe_variable(
        df = data.frame(A = NA),
        variable = list(A = "character"),
        name = name,
        force_na = FALSE
      ),
      check_dataframe_variable(
        df = data.frame(A = NA),
        variable = list(A = "character"),
        name = name
      )
    )
  })
})
