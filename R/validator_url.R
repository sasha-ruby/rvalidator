#' URL Validator
#'
#' Validates that a value is a valid URL.
#' @param value The value to validate.
#' @param protocols A character vector of allowed protocols (e.g., c("http", "https")). Default is c("http", "https").
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' url_validator("https://www.example.com")
#' url_validator("ftp://example.com", protocols = c("ftp"))
#' url_validator("invalid_url")
url_validator <- function(value, protocols = c("http", "https"), message = NULL) {
  protocol_pattern <- paste(protocols, collapse = "|")
  pattern <- glue("^(?:{protocol_pattern})://[\\w.-]+(?:\\.[\\w.-]+)+[/\\w\\-.~?&=%+#]*$")
  is_valid <- grepl(pattern, value, perl = TRUE)
  default_message <- glue("The value '{value}' is not a valid URL.")
  message <- if (is_valid) "" else (message %||% default_message)
  list(is_valid = is_valid, message = message)
}
