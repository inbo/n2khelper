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
        sprintf(
          paste0("%.", sha1_digits("base"), "e"),
          zapsmall(x.numeric, digits = sha1_digits("zapsmall"))
        ),
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
      digest::digest(sapply(x.dataframe, get_sha1), algo = "sha1")
    )
    expect_identical(
      get_sha1(x.matrix.num),
      get_sha1(as.vector(x.matrix.num))
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

  lm.model.0 <- lm(weight ~ Time, data = ChickWeight)
  lm.model.1 <- lm(weight ~ 1, data = ChickWeight)
  glm.model.0 <- glm(weight ~ Time, data = ChickWeight, family = poisson)
  glm.model.1 <- glm(weight ~ 1, data = ChickWeight, family = poisson)

  anova.list <- list(
    lm = anova(lm.model.0, lm.model.1),
    glm = anova(glm.model.0, glm.model.1),
    glm.test = anova(glm.model.0, glm.model.1, test = "Chisq")
  )

  it("works with lm anova", {
    z <- apply(anova.list[["lm"]], 1, function(y){
      sprintf(
        paste0("%.", sha1_digits("coef"), "e"),
        zapsmall(y, digits = sha1_digits("zapsmall"))
      )
    })
    expect_identical(
      get_sha1(anova.list[["lm"]]),
      get_sha1(z)
    )
  })
  it("works with glm anova", {
    z <- apply(anova.list[["glm"]], 1, function(y){
      sprintf(
        paste0("%.", sha1_digits("coef"), "e"),
        zapsmall(y, digits = sha1_digits("zapsmall"))
      )
    })
    expect_identical(
      get_sha1(anova.list[["glm"]]),
      get_sha1(z)
    )
    z <- apply(anova.list[["glm.test"]], 1, function(y){
      sprintf(
        paste0("%.", sha1_digits("coef"), "e"),
        zapsmall(y, digits = sha1_digits("zapsmall"))
      )
    })
    expect_identical(
      get_sha1(anova.list[["glm.test"]]),
      get_sha1(z)
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
    "37dadeab8d8ce7611f230f9524c1e8ab751c4a6a",
    "37dadeab8d8ce7611f230f9524c1e8ab751c4a6a",
    "37dadeab8d8ce7611f230f9524c1e8ab751c4a6a",
    "ad8d56a358f1c91717e506012ea43a9b700a8d51",
    "0f0714f1142eed0701166aa2d6dcdd798c8420e6",
    "1ff7ec513ca937d95071b47e30f851941a92bb1a",
    "1ff7ec513ca937d95071b47e30f851941a92bb1a",
    "1f9928593251410322823fefea8c3ef79b4d0254",
    "c799247ef7cc5eb0a3544aa1aef1039e270579a4",
    "692ff1b9390cfc01625d8dbb850d04426e193889",
    "4169b5d388e894b4098f28fa0b4fee47b37de2f2",
    "e2855241305df1870c68277a211261e5c7ed3425",
    "3bc1c85261b958307340b7a8a9fcff3e2586516b",
    "3c23872b9b4e17b16c1d640fe3c29f251202b012",
    "0fc188b4fd874e5ea411a241feb0e247faada054",
    "188710fe63fedb3f4637db5eeb2ecdbc824aa179",
    "5a060548dfd4768ad815793f06bcde05ff6a3675",
    "5a6c79d7f2427ea997531da05940fc46daa9d21d",
    "25228aa01875f7c88b51c299a332c6bd82257d06",
    "51fe9849f2b30d02c73cd7870d5d9b3a19e83654",
    "c165458381d503502e811a153f262fe6a1dfa55e",
    "692ff1b9390cfc01625d8dbb850d04426e193889",
    "4169b5d388e894b4098f28fa0b4fee47b37de2f2",
    "e2855241305df1870c68277a211261e5c7ed3425",
    "3bc1c85261b958307340b7a8a9fcff3e2586516b",
    "3c23872b9b4e17b16c1d640fe3c29f251202b012",
    "0fc188b4fd874e5ea411a241feb0e247faada054",
    "1ff7ec513ca937d95071b47e30f851941a92bb1a",
    "4d1e599d09ad910c1d489011fcd0a323504d891e",
    "b68b33f773c469ad4545f178bb008eaae1f3f364",
    "d219c0181a373e300b3105f725c38f2c671ad8e4",
    "3abb744a28366898175dcbe82ca119aa9613d49c",
    "11c63e11fb8461ff0812e46f2f0786b4a001c1db",
    "896d064f1c18c01700cef7d69dcb70287a786994",
    "32183afb55a90cd1b4cc539371a9c72fef2218a1",
    "cee887a8ca753e55af23d30324eff1ae22a2e8c2",
    "f4fa233a6047562a12d23c2ca79b0b5ff769c738",
    "6e9a4aae63eb62ec7e653bd8d0602d096383dfd7",
    "cc3179470d71545bf5a393a2105d738c121f787b",
    "83f09bbda5d85de611c0bdd6113e731e9ea99ec2",
    "f4fa233a6047562a12d23c2ca79b0b5ff769c738",
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
    "9fe97bacc996ee095ccf4c86591b78cdcf3a62f1",
    "4f12516cb64820d679e688fceddae8b3a1dcfdab",
    "c758ce6551dba2c4d83720d128be35b4435de629"
  )
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    for (i in seq_along(test.element)) {
      expect_identical(
        get_sha1(test.element[[i]]),
        correct[i],
        label = paste0("test.element[[", i, "]]")
      )
    }
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
