#' Password Strength Validator
#'
#' Validates that a password meets strength requirements.
#' @param value The password to validate.
#' @param min_length Minimum length. Default is 8.
#' @param min_upper Minimum number of uppercase letters. Default is 1.
#' @param min_lower Minimum number of lowercase letters. Default is 1.
#' @param min_digit Minimum number of digits. Default is 1.
#' @param min_special Minimum number of special characters. Default is 1.
#' @param custom_message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' password_strength("P@ssw0rd!")
#' password_strength("weakpassword", custom_message = "Password is too weak.")
password_strength <- function(value, min_length = 8, min_upper = 1, min_lower = 1,
                              min_digit = 1, min_special = 1, custom_message = NULL) {
  # Helper function to count matches
  count_matches <- function(pattern, value) {
    matches <- gregexpr(pattern, value, perl = TRUE)
    regmatches_list <- regmatches(value, matches)
    return(length(unlist(regmatches_list)))
  }

  # Initialize validity and messages
  is_valid <- TRUE
  messages <- c()

  # Check each criterion
  if (nchar(value) < min_length) {
    is_valid <- FALSE
    if (is.null(custom_message)) {
      messages <- c(messages, glue("The password must be at least {min_length} characters long."))
    }
  }

  num_upper <- count_matches("[A-Z]", value)
  if (num_upper < min_upper) {
    is_valid <- FALSE
    if (is.null(custom_message)) {
      messages <- c(messages, glue("The password must contain at least {min_upper} uppercase letter(s)."))
    }
  }

  num_lower <- count_matches("[a-z]", value)
  if (num_lower < min_lower) {
    is_valid <- FALSE
    if (is.null(custom_message)) {
      messages <- c(messages, glue("The password must contain at least {min_lower} lowercase letter(s)."))
    }
  }

  num_digit <- count_matches("[0-9]", value)
  if (num_digit < min_digit) {
    is_valid <- FALSE
    if (is.null(custom_message)) {
      messages <- c(messages, glue("The password must contain at least {min_digit} digit(s)."))
    }
  }

  num_special <- count_matches("[^A-Za-z0-9]", value)
  if (num_special < min_special) {
    is_valid <- FALSE
    if (is.null(custom_message)) {
      messages <- c(messages, glue("The password must contain at least {min_special} special character(s)."))
    }
  }

  # Determine final message
  if (!is_valid) {
    if (!is.null(custom_message)) {
      final_message <- custom_message
    } else {
      final_message <- paste(messages, collapse = " ")
    }
  } else {
    final_message <- ""
  }

  list(is_valid = is_valid, message = final_message)
}
