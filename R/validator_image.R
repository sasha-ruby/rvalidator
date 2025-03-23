#' Image Validator
#'
#' Validates that a file is an image and meets specified criteria.
#' @param file The file to validate (from Shiny's `input$file`).
#' @param max_size Maximum file size in bytes. Default is Inf.
#' @param allowed_types Allowed image types. Default is c("image/png", "image/jpeg", "image/jpg", "image/gif").
#' @param max_width Maximum width in pixels. Default is Inf.
#' @param max_height Maximum height in pixels. Default is Inf.
#' @param message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' # In a Shiny app, use image_validator(input$file)
image_validator <- function(file, max_size = Inf, allowed_types = c("image/png", "image/jpeg", "image/jpg", "image/gif"), max_width = Inf, max_height = Inf, message = NULL) {
  is_valid <- TRUE
  messages <- c()

  if (is.null(file) || nrow(file) == 0) {
    is_valid <- FALSE
    default_message <- "No file uploaded."
    messages <- c(messages, message %||% default_message)
  } else {
    if (file$size > max_size) {
      is_valid <- FALSE
      default_message <- glue("The image '{file$name}' exceeds the maximum size of {max_size} bytes.")
      messages <- c(messages, message %||% default_message)
    }
    if (!file$type %in% allowed_types) {
      is_valid <- FALSE
      default_message <- glue("The file '{file$name}' is not an allowed image type.")
      messages <- c(messages, message %||% default_message)
    } else {
      img <- image_read(file$datapath)
      info <- image_info(img)
      width <- info$width
      height <- info$height
      if (width > max_width) {
        is_valid <- FALSE
        default_message <- glue("The image '{file$name}' exceeds the maximum width of {max_width} pixels.")
        messages <- c(messages, message %||% default_message)
      }
      if (height > max_height) {
        is_valid <- FALSE
        default_message <- glue("The image '{file$name}' exceeds the maximum height of {max_height} pixels.")
        messages <- c(messages, message %||% default_message)
      }
    }
  }

  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}
