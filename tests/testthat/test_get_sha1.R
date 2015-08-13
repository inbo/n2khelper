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

  test.element <- list(
    # NULL
    NULL,
    # empty vector
    logical(0), integer(0), numeric(0), character(0),
    # scalar
    TRUE, FALSE, 1L, 1, "a",
    # vector
    c(TRUE, FALSE), 1:10, seq(1, 10, length = 7), letters, factor(letters)
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
    "692ff1b9390cfc01625d8dbb850d04426e193889",
    "8ecd6c9b9ee20692440f0283ad257165ec7ae75d",
    "87ff39278d0ccb8990a96f66735ed3817511a340",
    "174b54126178f63dcfd088a201f0c014dbb5d3b0",
    "02031b6ef7804a28e10358985a91acca8315e989"
  )
  it("return the same SHA1 on both 32-bit and 64-bit OS", {
    expect_identical(
      correct,
      sapply(test.element, get_sha1)
    )
  })
})
