test_that("no_suspicious_characters works correctly", {
  # Valid input
  expect_true(no_suspicious_characters("Normal string")$is_valid)

  # Invalid input with zero-width space
  expect_false(no_suspicious_characters("String with zero-width space\u200B")$is_valid)

  # Invalid input with zero-width non-joiner
  expect_false(no_suspicious_characters("String with zero-width non-joiner\u200C")$is_valid)

  # Invalid input with zero-width joiner
  expect_false(no_suspicious_characters("String with zero-width joiner\u200D")$is_valid)

  # Invalid input with byte order mark
  expect_false(no_suspicious_characters("String with BOM\uFEFF")$is_valid)

  # Invalid input with control character
  expect_false(no_suspicious_characters("String with control character\x1F")$is_valid)

  # Custom message
  result <- no_suspicious_characters("Invalid\u200B", message = "Suspicious characters detected.")
  expect_false(result$is_valid)
  expect_equal(result$message, "Suspicious characters detected.")
})
