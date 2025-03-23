#' Range Validator
#'
#' Validates that a numeric value falls within a specified range.
#' @param value The value to validate.
#' @param min The minimum value.
#' @param max The maximum value.
#' @param message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' range_validator(5, min = 1, max = 10)
#' range_validator(15, max = 10)
range_validator <- function(value, min = NULL, max = NULL, message = NULL) {
  library(glue)
  is_valid <- TRUE
  messages <- c()

  if (!is.null(min) && value < min) {
    is_valid <- FALSE
    default_message <- glue("The value '{value}' is less than the minimum {min}.")
    messages <- c(messages, message %||% default_message)
  }
  if (!is.null(max) && value > max) {
    is_valid <- FALSE
    default_message <- glue("The value '{value}' is greater than the maximum {max}.")
    messages <- c(messages, message %||% default_message)
  }

  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}
