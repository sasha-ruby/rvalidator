#' Not Compromised Password Validator
#'
#' Validates that a password has not been compromised in known data breaches.
#' Uses the Have I Been Pwned API.
#' @param value The password to validate.
#' @param message Custom error message. If NULL, a default message is used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @note This function uses the Have I Been Pwned API and the k-anonymity model.
#' @examples
#' not_compromised_password("P@ssw0rd!")
not_compromised_password <- function(value, message = NULL) {
  library(glue)
  library(digest)
  library(httr)

  sha1_hash <- toupper(digest(value, algo = "sha1", serialize = FALSE))
  prefix <- substr(sha1_hash, 1, 5)
  suffix <- substr(sha1_hash, 6, nchar(sha1_hash))

  url <- glue("https://api.pwnedpasswords.com/range/{prefix}")
  response <- GET(url)

  if (response$status_code != 200) {
    stop("Error querying the Have I Been Pwned API.")
  }

  hashes <- content(response, "text")
  compromised <- grepl(suffix, hashes, fixed = TRUE)

  is_valid <- !compromised
  default_message <- "This password has been compromised in a data breach."
  message <- if (is_valid) "" else (message %||% default_message)

  list(is_valid = is_valid, message = message)
}
