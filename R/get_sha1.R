#' Define the number of digits to use when rounding numeric values
sha1_digits <- function(){
  15
}

#' Calculate a SHA1 hash of an object
#' @param x the object to calculate the SHA1
#' @name get_sha1
#' @rdname get_sha1
#' @exportMethod get_sha1
#' @docType methods
#' @importFrom methods setGeneric
setGeneric(
  name = "get_sha1",
  def = function(x){
    standard.generic("get_sha1")
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "ANY",
  definition = function(x){
    digest(x, algo = "sha1")
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "list",
  definition = function(x){
    digest(
      sapply(x, get_sha1),
      algo = "sha1"
    )
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "numeric",
  definition = function(x){
    digest(
      # needed to make results comparable between 32-bit and 64-bit
      signif(x, digits = sha1_digits()),
      algo = "sha1"
    )
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "matrix",
  definition = function(x){
    # needed to make results comparable between 32-bit and 64-bit
    if (class(x[1, 1]) == "numeric") {
      x <- signif(x, digits = sha1_digits())
    }
    digest(x, algo = "sha1")
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "data.frame",
  definition = function(x){
    # needed to make results comparable between 32-bit and 64-bit
    numeric.field <- which(sapply(x, class) == "numeric")
    x[, numeric.field] <- signif(x[, numeric.field], digits = sha1_digits())
    digest(x, algo = "sha1")
  }
)
