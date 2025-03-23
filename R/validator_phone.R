#' Phone Number Validator
#'
#' Validates that a value is a valid North American phone number.
#' Accepts formats like: (123) 456-7890, 123-456-7890, 1234567890
#' @param value The value to validate.
#' @param require_area_code Logical. If TRUE, requires a 3-digit area code. Default is TRUE.
#' @param allow_country_code Logical. If TRUE, allows optional +1 or 1 prefix. Default is TRUE.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' phone_validator("(123) 456-7890")
#' phone_validator("123-456-7890")
#' phone_validator("+1-123-456-7890")
#' phone_validator("456-7890", require_area_code = FALSE)
phone_validator <- function(value, require_area_code = TRUE, 
                          allow_country_code = TRUE, message = NULL) {
  # Remove all non-numeric characters
  clean_number <- gsub("[^0-9]", "", value)
  
  # Initialize validation
  is_valid <- TRUE
  messages <- c()
  
  # Check length after cleaning
  n_digits <- nchar(clean_number)
  
  # Handle country code
  if (n_digits == 11 && substr(clean_number, 1, 1) == "1") {
    if (!allow_country_code) {
      is_valid <- FALSE
      messages <- c(messages, message %||% "Country code not allowed")
    }
    clean_number <- substr(clean_number, 2, 11)
    n_digits <- 10
  }
  
  # Check basic length requirements
  expected_length <- if (require_area_code) 10 else 7
  if (n_digits != expected_length) {
    is_valid <- FALSE
    messages <- c(messages, message %||% 
      glue("Phone number must be {expected_length} digits"))
  }
  
  # Check area code if required
  if (require_area_code && n_digits >= 3) {
    area_code <- substr(clean_number, 1, 3)
    if (area_code %in% c("000", "555", "911")) {
      is_valid <- FALSE
      messages <- c(messages, message %||% 
        glue("Invalid area code: {area_code}"))
    }
  }
  
  # Check exchange code (next 3 digits)
  if (n_digits >= 6) {
    exchange <- substr(clean_number, if (require_area_code) 4 else 1, 
                      if (require_area_code) 6 else 3)
    if (exchange == "000" || exchange == "555") {
      is_valid <- FALSE
      messages <- c(messages, message %||% 
        glue("Invalid exchange code: {exchange}"))
    }
  }
  
  # Return validation result
  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}