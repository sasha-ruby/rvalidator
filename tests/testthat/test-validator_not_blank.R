test_that("not_blank validator works correctly", {
  expect_true(not_blank("Hello")$is_valid)
  expect_false(not_blank("")$is_valid)
  expect_false(not_blank(NULL)$is_valid)
})
