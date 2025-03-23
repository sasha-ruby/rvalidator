test_that("length_validator works correctly", {
  # Valid length within bounds
  expect_true(length_validator("Hello", min = 3, max = 10)$is_valid)

  # Too short
  expect_false(length_validator("Hi", min = 3)$is_valid)

  # Too long
  expect_false(length_validator("This is a very long string", max = 10)$is_valid)

  # No min or max specified (should always be valid)
  expect_true(length_validator("Any length string")$is_valid)

  # Custom message
  result <- length_validator("Hi", min = 5, message = "String is too short.")
  expect_false(result$is_valid)
  expect_equal(result$message, "String is too short.")
})
