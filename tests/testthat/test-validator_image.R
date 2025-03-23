test_that("image_validator works correctly", {
  temp_img <- image_blank(width = 800, height = 600, color = "white")
  temp_file <- tempfile(fileext = ".png")
  image_write(temp_img, path = temp_file, format = "png")

  test_file <- data.frame(
    name = basename(temp_file),
    size = file.size(temp_file),
    type = "image/png",
    datapath = temp_file,
    stringsAsFactors = FALSE
  )

  expect_true(image_validator(test_file)$is_valid)

  # Exceeding max width
  expect_false(image_validator(test_file, max_width = 500)$is_valid)

  # Exceeding max height
  expect_false(image_validator(test_file, max_height = 400)$is_valid)

  # Disallowed type
  test_file$type <- "image/svg+xml"
  expect_false(image_validator(test_file)$is_valid)

  unlink(temp_file)
})
