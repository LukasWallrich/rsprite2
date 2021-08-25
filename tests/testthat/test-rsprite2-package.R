if (!interactive()) {
  test_that("fail without req package", {
    expect_error(.check_req_packages("abcdefxyz"))
  })
}
