context("read object from environment")
describe("read_object_environment()", {
  object <- "test"
  missing_object <- "junk"
  value <- TRUE
  env <- new.env()
  assign(x = object, value = value, envir = env)

  it("checks if env is an environment", {
    expect_error(
      read_object_environment(object = object, env = object),
      "env does not inherit from class environment"
    )
  })
  it("returns the object or NULL if the object doesn't exists", {
    expect_identical(
      read_object_environment(object = object, env = env),
      value
    )
    expect_null(
      suppressWarnings(
        read_object_environment(object = missing_object, env = env)
      )
    )
  })
  it("returns a warning when the object is missing", {
    expect_that(
      read_object_environment(object = missing_object, env = env),
      gives_warning(paste(missing_object, "doesn't exists in env"))
    )
    expect_that(
      read_object_environment(object = missing_object, env = env, warn = TRUE),
      gives_warning(paste(missing_object, "doesn't exists in env"))
    )
    expect_silent(
      read_object_environment(object = missing_object, env = env, warn = FALSE)
    )
  })
})
