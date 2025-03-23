test_that("phone_validator accepts valid formats", {
  expect_true(phone_validator("(123) 456-7890")$is_valid)
  expect_true(phone_validator("123-456-7890")$is_valid)
  expect_true(phone_validator("1234567890")$is_valid)
  expect_true(phone_validator("+1-123-456-7890")$is_valid)
  expect_true(phone_validator("456-7890", require_area_code = FALSE)$is_valid)
})

test_that("phone_validator rejects invalid formats", {
  expect_false(phone_validator("123-456")$is_valid)
  expect_false(phone_validator("abc-def-ghij")$is_valid)
  expect_false(phone_validator("000-456-7890")$is_valid)
  expect_false(phone_validator("123-555-7890")$is_valid)
  expect_false(phone_validator("911-456-7890")$is_valid)
})

test_that("phone_validator handles area code requirements", {
  expect_true(phone_validator("456-7890", require_area_code = FALSE)$is_valid)
  expect_false(phone_validator("456-7890", require_area_code = TRUE)$is_valid)
})

test_that("phone_validator handles country code settings", {
  expect_true(phone_validator("+1-123-456-7890", allow_country_code = TRUE)$is_valid)
  expect_false(phone_validator("+1-123-456-7890", allow_country_code = FALSE)$is_valid)
})