test_that("file_validator works correctly", {
  test_file <- data.frame(
    name = "test.txt",
    size = 1024,
    type = "text/plain",
    datapath = tempdir(),
    stringsAsFactors = FALSE
  )

  expect_true(file_validator(test_file)$is_valid)

  # Exceeding max size
  test_file$size <- 2 * 1024 * 1024  # 2 MB
  expect_false(file_validator(test_file, max_size = 1024 * 1024)$is_valid)

  # Disallowed type
  test_file$size <- 1024
  expect_false(file_validator(test_file, allowed_types = c("image/png"))$is_valid)
})
