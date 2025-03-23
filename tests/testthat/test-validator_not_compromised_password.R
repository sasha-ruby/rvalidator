test_that("not_compromised_password works correctly", {
  # Use a known compromised password
  expect_false(not_compromised_password("password")$is_valid)
  # Use a strong password (Note: There's a chance any password may be compromised)
  expect_true(not_compromised_password("A3$d9!fG6k")$is_valid)
})
