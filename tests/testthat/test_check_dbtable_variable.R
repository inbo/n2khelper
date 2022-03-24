test_that("checks if table is a single character", {
  skip_on_cran()
  skip_if(as.logical(Sys.getenv("CHECKLIST_OS", FALSE)))
  junk <- "junk"
  table <- "location"
  variable <- c("external_code", "description")
  error <- TRUE
  channel <- connect_ut_db()
  expect_error(
    check_dbtable_variable(
      table = integer(0), variable = variable, channel = channel,
      error = error
    ),
    "table is not a string"
  )
  expect_error(
    check_dbtable_variable(
      table = NA, variable = variable, channel = channel, error = error
    ),
    "table is not a string"
  )
  expect_error(
    check_dbtable_variable(
      table = character(2), variable = variable, channel = channel,
      error = error
    ),
    "table is not a string"
  )
  expect_error(
    check_dbtable_variable(
      table = character(0), variable = variable, channel = channel,
      error = error
    ),
    "table is not a string"
  )
  DBI::dbDisconnect(channel)
})

test_that("checks if table exists", {
  skip_on_cran()
  skip_if(as.logical(Sys.getenv("CHECKLIST_OS", FALSE)))
  junk <- "junk"
  table <- "location"
  variable <- c("external_code", "description")
  error <- TRUE
  channel <- connect_ut_db()
  expect_error(
    check_dbtable_variable(
      table = junk, variable = variable, channel = channel, error = error
    ),
    sprintf("Table\\(s\\) %s not found in schema public on database", junk)
  )
})

test_that("checks if channel is on ODBC connection", {
  skip_on_cran()
  skip_if(as.logical(Sys.getenv("CHECKLIST_OS", FALSE)))
  junk <- "junk"
  table <- "location"
  variable <- c("external_code", "description")
  error <- TRUE
  channel <- connect_ut_db()
  expect_error(
    check_dbtable_variable(
      table = table, variable = variable, channel = junk, error = error
    ),
    "channel does not inherit from class DBIConnection"
  )
  DBI::dbDisconnect(channel)
})

test_that(
  "checks if variable is a non-empty character without missing values", {
  skip_on_cran()
  skip_if(as.logical(Sys.getenv("CHECKLIST_OS", FALSE)))
  junk <- "junk"
  table <- "location"
  variable <- c("external_code", "description")
  error <- TRUE
  channel <- connect_ut_db()
  expect_error(
    check_dbtable_variable(
      table = table, variable = integer(0), channel = channel, error = error
    ),
    "variable must be character"
  )
  expect_error(
    check_dbtable_variable(
      table = table, variable = c(variable, NA), channel = channel,
      error = error
    ),
    "missing values in object"
  )
  expect_error(
    check_dbtable_variable(
      table = table, variable = character(0), channel = channel, error = error
    ),
    "'variable' must contain at least one value"
  )
  DBI::dbDisconnect(channel)
})

test_that("gives correct output", {
  skip_on_cran()
  skip_if(as.logical(Sys.getenv("CHECKLIST_OS", FALSE)))
  junk <- "junk"
  table <- "location"
  variable <- c("external_code", "description")
  error <- TRUE
  channel <- connect_ut_db()
  expect_true(
    check_dbtable_variable(
      table = table, variable = variable, channel = channel, error = error
    )
  )
  expect_false(
    check_dbtable_variable(
      table = table, variable = junk, channel = channel, error = FALSE
    )
  )
  expect_false(
    check_dbtable_variable(
      table = table, variable = junk, channel = channel, error = FALSE
    )
  )
  expect_error(
    check_dbtable_variable(
      table = table, variable = junk, channel = channel, error = TRUE
    ),
    paste0(
      "Variable\\(s\\) missing from '", table, "': ",
      paste(junk, collapse = ", ")
    )
  )
  DBI::dbDisconnect(channel)
})
