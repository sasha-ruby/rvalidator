#' Password Strength Validator
#'
#' Validates that a password meets strength requirements.
#' @param value The password to validate.
#' @param min_length Minimum length. Default is 8.
#' @param min_upper Minimum number of uppercase letters. Default is 1.
#' @param min_lower Minimum number of lowercase letters. Default is 1.
#' @param min_digit Minimum number of digits. Default is 1.
#' @param min_special Minimum number of special characters. Default is 1.
#' @param message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @examples
#' password_strength("P@ssw0rd!")
#' password_strength("weakpassword")
password_strength <- function(value, min_length = 8, min_upper = 1, min_lower = 1, min_digit = 1, min_special = 1, message = NULL) {
  library(glue)
  is_valid <- TRUE
  messages <- c()

  if (nchar(value) < min_length) {
    is_valid <- FALSE
    default_message <- glue("The password '{value}' must be at least {min_length} characters long.")
    messages <- c(messages, message %||% default_message)
  }

  if (length(gregexpr("[A-Z]", value)[[1]]) - 1 < min_upper) {
    is_valid <- FALSE
    default_message <- glue("The password must contain at least {min_upper} uppercase letter(s).")
    messages <- c(messages, message %||% default_message)
  }

  if (length(gregexpr("[a-z]", value)[[1]]) - 1 < min_lower) {
    is_valid <- FALSE
    default_message <- glue("The password must contain at least {min_lower} lowercase letter(s).")
    messages <- c(messages, message %||% default_message)
  }

  if (length(gregexpr("[0-9]", value)[[1]]) - 1 < min_digit) {
    is_valid <- FALSE
    default_message <- glue("The password must contain at least {min_digit} digit(s).")
    messages <- c(messages, message %||% default_message)
  }

  if (length(gregexpr("[^A-Za-z0-9]", value)[[1]]) - 1 < min_special) {
    is_valid <- FALSE
    default_message <- glue("The password must contain at least {min_special} special character(s).")
    messages <- c(messages, message %||% default_message)
  }

  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}
