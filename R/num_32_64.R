#' Convert a numeric to a character string
#'
#' This function should give the same output on 32-bit and 64-bit systems
#' @param x the numeric to convert
#' @param digits the approximate number of significant digits in base 10. Will
#'    be converted to a base 16 equivalent
#' @param zapsmall the apporixmate negative magnitute of the smallest relevant
#'    digit. Will be converted to a base 2 equivalent. Values smaller than this
#'    number are equivalent to 0.
#' @return a character vector
#' @export
#' @importFrom assertthat assert_that is.count
#' @importFrom dplyr %>%
num_32_64 <- function(x, digits = 6, zapsmall = 7){
  assert_that(is.numeric(x))
  assert_that(is.count(digits))
  assert_that(is.count(zapsmall))

  if (length(x) == 0) {
    return(character(0))
  }
  x.na <- is.na(x)
  if (all(x.na)) {
    return(x)
  }
  output <- rep(NA, length(x))

  x.hex <- sprintf("%a", x[!x.na])
  exponent <- gsub("^.*p", "", x.hex) %>% as.integer()

  # detect "small" numbers
  zapsmall.hex <- floor(log2(10 ^ -zapsmall))
  zero <- x.hex == sprintf("%a", 0) | exponent <= zapsmall.hex
  if (any(zero)) {
    output[!x.na][zero] <- "0"
    if (all(zero)) {
      return(output)
    }
  }

  digits.hex <- ceiling(log(10 ^ digits, base = 16))
  mantissa <- x.hex[!zero] %>% # select "large" numbers
    gsub(pattern = ".*x1\\.{0,1}", replacement = "") %>% # select mantissa
    gsub(pattern = "p.*$", replacement = "") %>% # select mantissa
    substring(1, digits.hex) %>% # select required precision
    gsub(pattern = "0$", replacement = "") # remove potential trailing zero's
  negative <- x.hex[!zero] %>% grepl(pattern = "^-") %>% ifelse("-", "")
  output[!x.na][!zero] <- paste0(negative, mantissa, " ", exponent[!zero])
  return(output)
}
