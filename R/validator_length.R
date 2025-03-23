#' Length Validator
#'
#' Validates that the length of a string is within specified bounds.
#' @param value The value to validate.
#' @param min The minimum length.
#' @param max The maximum length.
#' @param message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' length_validator("Hello", min = 3, max = 10)
#' length_validator("Hi", min = 3)
length_validator <- function(value, min = NULL, max = NULL, message = NULL) {
  library(glue)
  n <- nchar(value)
  is_valid <- TRUE
  messages <- c()

  if (!is.null(min) && n < min) {
    is_valid <- FALSE
    default_message <- glue("The value '{value}' is too short. It should have {min} characters or more.")
    messages <- c(messages, message %||% default_message)
  }
  if (!is.null(max) && n > max) {
    is_valid <- FALSE
    default_message <- glue("The value '{value}' is too long. It should have {max} characters or less.")
    messages <- c(messages, message %||% default_message)
  }

  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}
