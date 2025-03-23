#' Regex Validator
#'
#' Validates that a value matches a regular expression.
#' @param value The value to validate.
#' @param pattern The regex pattern.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' regex_validator("abc123", "^[a-z]+[0-9]+$")
#' regex_validator("123abc", "^[a-z]+[0-9]+$")
regex_validator <- function(value, pattern, message = NULL) {
  is_valid <- grepl(pattern, value)
  default_message <- glue("The value '{value}' does not match the required format.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
