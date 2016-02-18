context("connect result")
describe("check_id()", {
  it("throws an error on the production version", {
    expect_error(
      connect_result(develop = FALSE),
      "Production database not yet defined"
    )
  })
  it("uses trusted authentication when username is missing or ''", {
    skip_on_cran()
    skip_on_os("linux")
    expect_is(
      connect_result(),
      "RODBC"
    )
    expect_is(
      connect_result(username = ""),
      "RODBC"
    )
  })
  it("uses username and password when supplied", {
    skip_on_cran()
    expect_is(
      connect_result(
        username = Sys.getenv("N2KRESULT_USERNAME"),
        password = Sys.getenv("N2KRESULT_PASSWORD")
      ),
      "RODBC"
    )
  })
})
