context("calculate sha1 fingerprints")
describe("get_sha1", {
  x.numeric <- 1.2345678901234567890123456789
  x.list <- list(letters, x.numeric)
  x.dataframe <- data.frame(
    X = letters,
    Y = x.numeric
  )
  x.matrix.num <- as.matrix(x.numeric)
  x.matrix.letter <- as.matrix(letters)
  x.dataframe.round <- x.dataframe
  x.dataframe.round$Y <- signif(x.dataframe.round$Y, n2khelper:::sha1_digits())
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
      digest::digest(signif(x.numeric, n2khelper:::sha1_digits()), algo = "sha1")
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
      digest::digest(signif(x.matrix.num, n2khelper:::sha1_digits()), algo = "sha1")
    )
    expect_that(
      get_sha1(x.matrix.letter),
      is_identical_to(digest::digest(x.matrix.letter, algo = "sha1"))
    )
  })
})
