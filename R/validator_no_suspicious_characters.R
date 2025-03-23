#' No Suspicious Characters Validator
#'
#' Validates that a string does not contain suspicious characters.
#' Suspicious characters include control characters (code points U+0000 to U+001F and U+007F)
#' and specific format characters (e.g., U+200B to U+200D, U+FEFF).
#' @param value The value to validate.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' # Valid input
#' no_suspicious_characters("Normal string")
#' # Invalid input with zero-width space
#' no_suspicious_characters("String with zero-width space\u200B")
no_suspicious_characters <- function(value, message = NULL) {
  library(glue)
  # Define the code points for suspicious characters
  suspicious_code_points <- c(
    0:31,   # Control characters U+0000 to U+001F
    127,    # Delete character U+007F
    8203,   # Zero-width space U+200B
    8204,   # Zero-width non-joiner U+200C
    8205,   # Zero-width joiner U+200D
    65279   # Byte Order Mark U+FEFF
  )
  # Convert the input string to code points
  code_points <- utf8ToInt(value)
  # Check if any code points are in the suspicious list
  is_valid <- !any(code_points %in% suspicious_code_points)
  default_message <- glue("The value '{value}' contains suspicious characters.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
