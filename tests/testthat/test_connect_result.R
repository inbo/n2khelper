context("connect result")
describe("check_id()", {
  it("throws an error on the production version", {
    expect_error(
      connect_result(
        username = Sys.getenv("N2KRESULT_USERNAME"),
        password = Sys.getenv("N2KRESULT_PASSWORD"),
        develop = FALSE
      ),
      "Production database not yet defined"
    )
  })
  it("uses trusted authentication when username is missing or ''", {
    skip_on_cran()
    skip_on_os("linux")
    skip_if_not(identical(Sys.getenv("RSPM"), ""))
    expect_is(
      connect_result(),
      "src"
    )
    expect_is(
      connect_result(username = ""),
      "src"
    )
  })
  it("uses username and password when supplied", {
    skip_on_cran()
    skip_if_not(identical(Sys.getenv("RSPM"), ""))
    expect_is(
      connect_result(
        username = Sys.getenv("N2KRESULT_USERNAME"),
        password = Sys.getenv("N2KRESULT_PASSWORD")
      ),
      "PostgreSQLConnection"
    )
  })
})
