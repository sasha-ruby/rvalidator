#' Date Validator
#'
#' Validates that a value is a valid date.
#' @param value The value to validate.
#' @param format The date format to validate against.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' date_validator("2021-12-31", format = "%Y-%m-%d")
#' date_validator("31/12/2021", format = "%Y-%m-%d")
date_validator <- function(value, format = "%Y-%m-%d", message = NULL) {
  library(glue)
  parsed_date <- as.Date(value, format = format)
  is_valid <- !is.na(parsed_date)
  default_message <- glue("The value '{value}' is not a valid date in the format '{format}'.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
