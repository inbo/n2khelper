context("calculate sha1 fingerprints")
describe("get_sha1", {
  x.numeric <- 1.2345678901234567890123456789
  x.list <- list(letters, x.numeric)
  x.dataframe <- data.frame(
    X = letters,
    Y = x.numeric,
    Z = factor(letters),
    stringsAsFactors = FALSE
  )
  x.matrix.num <- as.matrix(x.numeric)
  x.matrix.letter <- as.matrix(letters)
  x.dataframe.round <- x.dataframe
  x.dataframe.round$Y <- signif(x.dataframe.round$Y, n2khelper:::sha1_digits())
  x.factor <- factor(letters)


  set.seed(1345)
  dummy.data <- expand.grid(
    rep = seq_len(3),
    x = seq(0, 1, length = 10)
  )
  dummy.data$eta <- 0.9 + 2 * dummy.data$x
  dummy.data$y <- rpois(nrow(dummy.data), lambda = exp(dummy.data$eta))

  lm.model.0 <- lm(y ~ x, data = dummy.data)
  lm.model.1 <- lm(y ~ 1, data = dummy.data)
  glm.model.0 <- glm(y ~ x, data = dummy.data, family = poisson)
  glm.model.1 <- glm(y ~ 1, data = dummy.data, family = poisson)

  anova.list <- list(
    lm = anova(lm.model.0, lm.model.1),
    glm = anova(glm.model.0, glm.model.1)
  )

  it("tests using detailed numbers", {
    expect_that(
      identical(x.numeric, signif(x.numeric, n2khelper:::sha1_digits())),
      is_false()
    )
    expect_that(
      identical(x.matrix.num, signif(x.matrix.num, n2khelper:::sha1_digits())),
      is_false()
    )
  })
  it("returns the correct SHA1", {
    expect_identical(
      get_sha1(x.numeric),
      digest::digest(
        signif(x.numeric, n2khelper:::sha1_digits()),
        algo = "sha1"
      )
    )
    expect_that(
      get_sha1(letters),
      is_identical_to(digest::digest(letters, algo = "sha1"))
    )
    expect_that(
      get_sha1(x.list),
      is_identical_to(
        digest::digest(
          sapply(x.list, get_sha1),
          algo = "sha1"
        )
      )
    )
    expect_identical(
      get_sha1(x.dataframe),
      digest::digest(x.dataframe.round, algo = "sha1")
    )
    expect_identical(
      get_sha1(x.matrix.num),
      digest::digest(
        signif(x.matrix.num, n2khelper:::sha1_digits()),
        algo = "sha1"
      )
    )
    expect_that(
      get_sha1(x.matrix.letter),
      is_identical_to(digest::digest(x.matrix.letter, algo = "sha1"))
    )
    expect_identical(
      get_sha1(x.factor),
      digest::digest(x.factor, algo = "sha1")
    )
  })

  it("works with lm anova", {
    expect_identical(
      get_sha1(anova.list[["lm"]]),
      get_sha1(signif(unlist(anova.list[["lm"]]), 4))
    )
  })
  it("works with glm anova", {
    expect_identical(
      get_sha1(anova.list[["glm"]]),
      get_sha1(signif(unlist(anova.list[["glm"]]), 4))
    )
  })

  test.element <- list(
    # NULL
    NULL,
    # empty vector
    logical(0), integer(0), numeric(0), character(0),
    # scalar
    TRUE, FALSE, 1L, 1, "a",
    # date. Make sure to add the time zone. Otherwise the test might fail
    as.POSIXct("2015-01-02 03:04:06.07", tz = "UTC"),
    # vector
    c(TRUE, FALSE), 1:3, seq(0, 10, length = 4), letters[1:3],
    factor(letters[4:6]),
    as.POSIXct(c("2015-01-02 03:04:06.07", "1960-12-31 23:59:59"), tz = "UTC")
  )
  select.vector <- which(sapply(test.element, length) > 1)
  test.element <- c(
    test.element,
    # add a data.frame
    expand.grid(test.element[select.vector]),
    # add a list
    test.element[select.vector],
    # add matrices
    matrix(1:10),
    matrix(seq(0, 10, length = 4)),
    matrix(letters),
    anova.list
  )
  cat("\ncorrect <- c(\n")
  cat(
    sprintf("  \"%s\"", sapply(test.element, get_sha1)),
    sep = ",\n"
  )
  cat(")\n")
  correct <- c(
    "8d9c05ec7ae28b219c4c56edbce6a721bd68af82",
    "0df9019fab513592066cc292d412b9054575d844",
    "d90fb4a5e025f8681b061e6d9bd7fa1fcf17e960",
    "d90fb4a5e025f8681b061e6d9bd7fa1fcf17e960",
    "37dadeab8d8ce7611f230f9524c1e8ab751c4a6a",
    "ad8d56a358f1c91717e506012ea43a9b700a8d51",
    "0f0714f1142eed0701166aa2d6dcdd798c8420e6",
    "6c30934a0ea2c0473d37b6d8bb5b955b435a8bc1",
    "6c30934a0ea2c0473d37b6d8bb5b955b435a8bc1",
    "1f9928593251410322823fefea8c3ef79b4d0254",
    "c799247ef7cc5eb0a3544aa1aef1039e270579a4",
    "692ff1b9390cfc01625d8dbb850d04426e193889",
    "6fdccc872a60a9170f5cb5eee74312f4cbc384af",
    "8e67ddb0502dfe87c6d4341ab7592d98a154c676",
    "3bc1c85261b958307340b7a8a9fcff3e2586516b",
    "3c23872b9b4e17b16c1d640fe3c29f251202b012",
    "0fc188b4fd874e5ea411a241feb0e247faada054",
    "188710fe63fedb3f4637db5eeb2ecdbc824aa179",
    "d0ea20695ef505b589e92aacb7ce89ebbaae13a0",
    "6106eaf5ef08d1c7b6272ef26fd6612438bb6164",
    "25228aa01875f7c88b51c299a332c6bd82257d06",
    "51fe9849f2b30d02c73cd7870d5d9b3a19e83654",
    "c165458381d503502e811a153f262fe6a1dfa55e",
    "692ff1b9390cfc01625d8dbb850d04426e193889",
    "6fdccc872a60a9170f5cb5eee74312f4cbc384af",
    "8e67ddb0502dfe87c6d4341ab7592d98a154c676",
    "3bc1c85261b958307340b7a8a9fcff3e2586516b",
    "3c23872b9b4e17b16c1d640fe3c29f251202b012",
    "0fc188b4fd874e5ea411a241feb0e247faada054",
    "6c30934a0ea2c0473d37b6d8bb5b955b435a8bc1",
    "315a5aa84aa6cfa4f3fb4b652a596770be0365e8",
    "a05091ea911bb9665d685c99b42f20e08c8a1927",
    "f4477038cc95efbea855596fcc42fa28bc7dc9da",
    "fbda0789bf4ef2af3fac585ccfbcb169636e8eae",
    "e9a0e7aa182b91a3dc92d527cc4c9b1bbe5cb61a",
    "56b37df6bb681326fd528d8e1940a77cb6dd6928",
    "54e6f7d9f57078b6c6e4474a398a054dbf53a0e9",
    "507ef986ec269e4854ee486ba72e00d8becc5aa3",
    "50e27f6ab08034b9562926f6f13f8dbd233cfdc3",
    "25dd8f33676144d5e42427eff1c8546724d63d15",
    "c12b5c6e847d20a91a26f6f92e980094521c5d24",
    "92eae8531a5572ab2d155ca4d9c3b6d94a1e6615",
    "50e27f6ab08034b9562926f6f13f8dbd233cfdc3",
    "1f9928593251410322823fefea8c3ef79b4d0254",
    "ee6e7fdb03a0d35b3a6f499d0f8f610686551d51",
    "8e7f9fe32c49050c5ca146150fc58b93fbeea245",
    "e59165f73b7dc7e0d6ae94ec9aac9e8e95fd8a2c",
    "7f608bde8f0e308aa8866d737ddebbfae9674163",
    "86e99e22d003547538a5f446165488f7861fa2c3",
    "ce27dce0e84ad90d3e90e9b571a73720d0fb4890",
    "221799200137b7d72dfc4a618465bec71333a58b",
    "13b5c7533cccc95d2f7cd18df78ea78ed9111c02",
    "88b7c7c5f6921ec9e914488067552829a17a42a4",
    "6127e4cdbf02f18898554c037f0d4acb95c608ab",
    "984ca0fd9ed47ac08a31aeb88f9c9a5f905aeaa2",
    "954da0ea9a5d0aa42516beebc5542c638161934c",
    "7d1e34387808d9f726efbb1c8eb0819a115afb52",
    "2e21764867596d832896d9d28d6e6489a0b27249",
    "666881f1f74c498e0292ccf3d9d26089ee79dae7",
    "966dbbe6cf1c43ac784a8257b57896db9fd3f357",
    "4ab40e0c23010553e9e4c058ef58f50088f9e87c",
    "bfa0e51b33ebd3b9a823368b7e4c357b2b98790b",
    "fc1ba0a4718421f0050cc5e159703838f733aa59",
    "25cce9eca8abedab78a765b49e74fba77f4463d4",
    "9d453f3128cb2fd55684b979a11d47c97f12dc87",
    "d612108f47c8accbeffd2d9d54c1fa7f74fb432d",
    "ef60fa66262167e7a31398b16fa762151c6d1b28",
    "a235e3cc7109def777a99e660b9829cea48ce9a4",
    "d19d82f849bad81a39da932d3087a60c78de82c1",
    "5924db546cad58d2da6b667bbbef1f30afc268a3",
    "f894b10c26b93166e644aa4586d659ca0c23c00e"
  )
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    expect_identical(
      correct,
      unname(sapply(test.element, get_sha1))
    )
  })

  it(
"calculates the SHA1 of a list as the SHA1 of a vector of SHA1 of each element"
, {
    this.list <- list("a", "b")
    expect_identical(
      get_sha1(this.list),
      get_sha1(sapply(this.list, get_sha1))
    )
    this.list <- list(letters, this.list)
    expect_identical(
      get_sha1(this.list),
      get_sha1(sapply(this.list, get_sha1))
    )
})
})
