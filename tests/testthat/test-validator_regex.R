test_that("regex_validator works correctly", {
  # Value matches pattern
  expect_true(regex_validator("abc123", pattern = "^[a-z]+[0-9]+$")$is_valid)

  # Value does not match pattern
  expect_false(regex_validator("123abc", pattern = "^[a-z]+[0-9]+$")$is_valid)

  # Custom message
  result <- regex_validator("123abc", pattern = "^[a-z]+[0-9]+$", message = "Pattern mismatch.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Pattern mismatch.")
})
