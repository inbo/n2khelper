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

  cat(
    "x.numeric.sha1 <- \"", get_sha1(x.numeric), "\"\n",
    "x.list.sha1 <- \"", get_sha1(x.list), "\"\n",
    "x.dataframe.sha1 <- \"", get_sha1(x.dataframe), "\"\n",
    "x.matrix.num.sha1 <- \"", get_sha1(x.matrix.num), "\"\n",
    "x.matrix.letter.sha1 <- \"", get_sha1(x.matrix.letter), "\"\n",
    "x.factor.sha1 <- \"", get_sha1(x.factor), "\"\n",
    sep = ""
  )
  #32-bit, 14 digit, windows
  x.numeric.sha1 <- "007da406bbe456a2a3b6ab258efe17cf88206600"
  x.list.sha1 <- "f07bdb7e125c84f1b319506f0a65db047429a449"
  x.dataframe.sha1 <- "9c704b9c4b6cf7f9a09ed1c1053e0794cff16715"
  x.matrix.num.sha1 <- "a4234514920fb1d0aabc9bda021c03497d8f4b4a"
  x.matrix.letter.sha1 <- "c5100b97b0a506851834947557847ed1ddd2462e"
  x.factor.sha1 <- "02031b6ef7804a28e10358985a91acca8315e989"

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
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    expect_identical(get_sha1(x.numeric), x.numeric.sha1)
    expect_identical(get_sha1(x.list), x.list.sha1)
    expect_identical(get_sha1(x.dataframe), x.dataframe.sha1)
    expect_identical(get_sha1(x.matrix.num), x.matrix.num.sha1)
    expect_identical(get_sha1(x.matrix.letter), x.matrix.letter.sha1)
    expect_identical(get_sha1(x.factor), x.factor.sha1)
  })
})
