#' Read an object from an environment
#'
#' @param object the name of the object
#' @param env the environment
#' @param warn Issue a warning if the object is not found in the environment.
#'    Defaults to TRUE
#' @return the object or \code{NULL} is the object doesn't exists in the
#'    environment
#' @export
#' @importFrom utils hasName
#' @examples
#'   object <- "test"
#'   value <- TRUE
#'   env <- new.env()
#'   assign(x = object, value = value, envir = env)
#'   read_object_environment(object, env)

read_object_environment <- function(object, env, warn = TRUE){
  object <- check_single_character(object, name = "object")
  if (class(env) != "environment") {
    stop("env is not an environment")
  }
  warn <- check_single_logical(warn, name = "warn")

  if (!hasName(env, object)) {
    if (warn) {
      warning(object, " doesn't exists in env")
    }
    return(NULL)
  }
  return(get(object, envir = env))
}
