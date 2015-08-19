context("num_32_64")
expect_true(require(magrittr))
# zap small numbers to zero
zapsmall <- 1:10
border <- 2 ^ floor(log2(10 ^ -zapsmall))
for (i in seq_along(zapsmall)) {
  expect_identical(
    num_32_64(border[i] * -1:1, digits = 6, zapsmall = zapsmall[i]),
    rep("0", 3)
  )
}
# handle 0 correct
expect_identical(
  num_32_64(0),
  "0"
)

# digits are consistent
x <- pi
x.hex <- sapply(1:16, num_32_64, x = x)
x.hex <- x.hex[c(TRUE, diff(nchar(x.hex)) > 0)]
exponent <- x.hex %>%
  gsub("^[0-9a-f]* ", "", .) %>%
  unique()
expect_true(length(exponent) == 1)
mantissa <- x.hex %>%
  gsub(" [0-9]*$", "", .)
for (i in seq_along(mantissa) %>% head(-1)) {
  expect_true(
    mantissa[i] %>%
      paste0("^", ., ".*") %>%
      grepl(tail(mantissa, -i)) %>%
      all
  )
}

#it keeps NA values
x <- c(pi, NA, 0)
expect_identical(
  is.na(num_32_64(x)),
  is.na(x)
)
x <- c(pi, NA, pi)
expect_identical(
  is.na(num_32_64(x)),
  is.na(x)
)
x <- as.numeric(c(NA, NA, NA))
expect_identical(
  is.na(num_32_64(x)),
  is.na(x)
)

# handles empty vectors
expect_identical(
  num_32_64(numeric(0)),
  character(0)
)
