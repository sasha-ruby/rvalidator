test_that("password_strength works correctly", {
  # Should be valid
  expect_true(password_strength("P@ssw0rd!")$is_valid)

  # Missing uppercase letter
  expect_false(password_strength("p@ssw0rd!")$is_valid)

  # Missing lowercase letter
  expect_false(password_strength("P@SSW0RD!")$is_valid)

  # Missing digit
  expect_false(password_strength("Password!")$is_valid)

  # Missing special character
  expect_false(password_strength("Password1")$is_valid)

  # Too short
  expect_false(password_strength("Pass1!")$is_valid)

  # Custom message
  result <- password_strength("password", custom_message = "Password is too weak.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Password is too weak.")
})
