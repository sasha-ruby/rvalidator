#' Email Validator
#'
#' Validates that the input is a valid email address.
#' @param value The value to validate.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' email_validator("test@example.com")
#' email_validator("invalid-email")
email_validator <- function(value, message = NULL) {
  library(glue)
  pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
  is_valid <- grepl(pattern, value)
  default_message <- glue("The value '{value}' is not a valid email address.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
