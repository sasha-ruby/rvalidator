test_that("range_validator works correctly", {
  # Value within range
  expect_true(range_validator(5, min = 1, max = 10)$is_valid)

  # Value below minimum
  expect_false(range_validator(0, min = 1)$is_valid)

  # Value above maximum
  expect_false(range_validator(15, max = 10)$is_valid)

  # No min or max specified (should always be valid)
  expect_true(range_validator(100)$is_valid)

  # Custom message
  result <- range_validator(-1, min = 0, message = "Value must be non-negative.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Value must be non-negative.")
})
