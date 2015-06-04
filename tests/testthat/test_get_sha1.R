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
  x.dataframe.round$Y <- signif(x.dataframe.round$Y, 16)
  it("tests using detailed numbers", {
    expect_that(
      identical(x.numeric, signif(x.numeric, 16)),
      is_false()
    )
    expect_that(
      identical(x.matrix.num, signif(x.matrix.num, 16)),
      is_false()
    )
  })
  it("returns the correct SHA1", {
    expect_that(
      get_sha1(x.numeric),
      is_identical_to(digest::digest(signif(x.numeric, 16), algo = "sha1"))
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
    expect_that(
      get_sha1(x.dataframe),
      is_identical_to(digest::digest(x.dataframe.round, algo = "sha1"))
    )
    expect_that(
      get_sha1(x.matrix.num),
      is_identical_to(digest::digest(signif(x.matrix.num, 16), algo = "sha1"))
    )
    expect_that(
      get_sha1(x.matrix.letter),
      is_identical_to(digest::digest(x.matrix.letter, algo = "sha1"))
    )
  })
})