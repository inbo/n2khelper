#' Read an object from an environment
#'
#' @param object the name of the object
#' @param env the environment
#' @param warn Issue a warning if the object is not found in the environment.
#'    Defaults to TRUE
#' @return the object or \code{NULL} is the object doesn't exists in the
#'    environment
#' @export
#' @importFrom assertthat assert_that is.flag is.string noNA
#' @importFrom utils hasName
#' @examples
#'   object <- "test"
#'   value <- TRUE
#'   env <- new.env()
#'   assign(x = object, value = value, envir = env)
#'   read_object_environment(object, env)

read_object_environment <- function(object, env, warn = TRUE) {
  assert_that(is.string(object), noNA(object))
  assert_that(inherits(env, "environment"))
  assert_that(is.flag(warn), noNA(warn))

  if (hasName(env, object)) {
    return(get(object, envir = env))
  }
  if (warn) {
    warning(object, " doesn't exists in env")
  }
  return(NULL)
}
