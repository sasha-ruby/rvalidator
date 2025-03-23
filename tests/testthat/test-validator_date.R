test_that("date_validator works correctly", {
  # Valid date in default format
  expect_true(date_validator("2021-12-31")$is_valid)

  # Invalid date (format mismatch)
  expect_false(date_validator("31-12-2021")$is_valid)

  # Invalid date (non-existent date)
  expect_false(date_validator("2021-02-30")$is_valid)

  # Valid date in custom format
  expect_true(date_validator("31/12/2021", format = "%d/%m/%Y")$is_valid)

  # Invalid date in custom format
  expect_false(date_validator("12/31/2021", format = "%d/%m/%Y")$is_valid)

  # Valid leap year date
  expect_true(date_validator("2020-02-29")$is_valid)

  # Invalid leap year date
  expect_false(date_validator("2019-02-29")$is_valid)

  # Custom message
  result <- date_validator("invalid date", message = "Please enter a valid date.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Please enter a valid date.")
})
