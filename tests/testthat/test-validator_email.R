test_that("email_validator works correctly", {
  # Valid email addresses
  expect_true(email_validator("test@example.com")$is_valid)
  expect_true(email_validator("user.name+tag+sorting@example.com")$is_valid)

  # Invalid email addresses
  expect_false(email_validator("plainaddress")$is_valid)
  expect_false(email_validator("@missingusername.com")$is_valid)

  # Custom message
  result <- email_validator("invalid@", message = "Email format is incorrect.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Email format is incorrect.")
})
