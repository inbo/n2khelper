#' Define the number of digits to use when rounding numeric values
#' @param which The number of digits for each usage
#' @export
sha1_digits <- function(which = c("base", "zapsmall", "coef")){
  which <- match.arg(which)
  switch(which,
    base = 14L,
    zapsmall = 7L,
    coef = 6L # coef = 5L yields differences for some lmer models
  )
}

#' Replace tiny numbers with zero
#' @param x the numeric vector to check
#' @param digits The minimal magnitude $10^-{digits}$
#' @export
#' @importFrom assertthat assert_that is.count
zap_small <- function(x, digits = sha1_digits("zapsmall")){
  assert_that(is.numeric(x))
  assert_that(is.count(digits))

  x[abs(x) < 10 ^ -digits] <- 0
  return(x)
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
  signature = "anova",
  definition = function(x){
    z <- apply(x, 1, function(y){
      sprintf(
        paste0("%.", sha1_digits("coef"), "e"),
        zap_small(y, digits = sha1_digits("zapsmall"))
      )
    })
    get_sha1(z)
  }
)

#' @rdname get_sha1
#' @importFrom methods setMethod
#' @importFrom digest digest
setMethod(
  f = "get_sha1",
  signature = "factor",
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
      # signif() doesn't work in all situations
      sprintf(
        paste0("%.", sha1_digits("base"), "e"),
        zap_small(x, digits = sha1_digits("zapsmall"))
      ),
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
      get_sha1(as.vector(x))
    } else {
      digest(x, algo = "sha1")
    }
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
    digest(sapply(x, get_sha1), algo = "sha1")
  }
)
