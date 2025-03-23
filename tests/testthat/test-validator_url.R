test_that("url_validator works correctly", {
  expect_true(url_validator("https://www.example.com")$is_valid)
  expect_true(url_validator("http://example.com/path?query=param")$is_valid)
  expect_false(url_validator("invalid_url")$is_valid)
  expect_false(url_validator("ftp://example.com")$is_valid)
  expect_true(url_validator("ftp://example.com", protocols = c("ftp"))$is_valid)
})
