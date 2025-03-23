#' No Suspicious Characters Validator
#'
#' Validates that a string does not contain suspicious characters.
#' @param value The value to validate.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' no_suspicious_characters("Normal string")
#' no_suspicious_characters("String with zero-width space\u200B")
no_suspicious_characters <- function(value, message = NULL) {
  library(glue)
  suspicious_pattern <- "[\\x00-\\x1F\\x7F\\u200B\\u200C\\u200D\\uFEFF]"
  is_valid <- !grepl(suspicious_pattern, value)
  default_message <- glue("The value '{value}' contains suspicious characters.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
