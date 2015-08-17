context("zap_small")
no.zap <- c(1, 1e-7)
zap <- c(1e-8, 9e-8, 9.99999999999999e-8)
expect_identical(
  zap_small(no.zap, 7),
  no.zap
)
expect_identical(
  zap_small(-no.zap, 7),
  -no.zap
)
expect_identical(
  zap_small(zap, 7),
  rep(0, length(zap))
)
expect_identical(
  zap_small(-zap, 7),
  rep(0, length(zap))
)
