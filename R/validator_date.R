#' Date Validator
#'
#' Validates that a value is a valid date matching the specified format.
#' @param value The value to validate.
#' @param format The date format to validate against (default is "%Y-%m-%d").
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' date_validator("2021-12-31")
#' date_validator("31/12/2021", format = "%d/%m/%Y")
#' date_validator("invalid date", message = "Please enter a valid date.")
date_validator <- function(value, format = "%Y-%m-%d", message = NULL) {
  library(glue)
  # Use strptime for parsing
  parsed_date <- strptime(value, format = format)
  # Check if parsing was successful and the formatted date matches the input
  is_valid <- !is.na(parsed_date) && format(parsed_date, format) == value
  default_message <- glue("The value '{value}' is not a valid date in the format '{format}'.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
