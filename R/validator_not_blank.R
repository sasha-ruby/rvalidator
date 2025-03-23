#' NotBlank Validator
#'
#' Validates that the input is not NULL or an empty string.
#' @param value The value to validate.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' not_blank("Hello")
#' not_blank("")
not_blank <- function(value, message = NULL) {
  library(glue)
  is_valid <- !(is.null(value) || trimws(value) == "")
  default_message <- glue("The value '{value}' should not be blank.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
