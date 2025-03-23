#' Choice Validator
#'
#' Validates that a value is within a set of allowed choices.
#' @param value The value to validate.
#' @param choices A vector of allowed choices.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' choice_validator("apple", choices = c("apple", "banana", "cherry"))
#' choice_validator("orange", choices = c("apple", "banana", "cherry"))
choice_validator <- function(value, choices, message = NULL) {
  is_valid <- value %in% choices
  default_message <- glue("The value '{value}' is not a valid choice.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
