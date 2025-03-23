test_that("choice_validator works correctly", {
  # Valid choice
  expect_true(choice_validator("apple", choices = c("apple", "banana", "cherry"))$is_valid)

  # Invalid choice
  expect_false(choice_validator("orange", choices = c("apple", "banana", "cherry"))$is_valid)

  # Custom message
  result <- choice_validator("orange", choices = c("apple", "banana"), message = "Invalid fruit selected.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Invalid fruit selected.")
})
