context("connect result")
describe("check_id()", {
  it ("throws an error on the production version") {
    expect_error(
      connect_result(develop = FALSE),
      "Production database not yet defined"
    )
  }
  it ("uses trusted authentication when username is missing or ''") {
    skip_on_cran()
    skip_on_os("unix")
    expect_is(
      connect_result(),
      "RODBC"
    )
    expect_is(
      connect_result(username = ""),
      "RODBC"
    )
  }
})
