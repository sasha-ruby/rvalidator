#' File Validator
#'
#' Validates that a file meets specified criteria.
#' @param file The file to validate (from Shiny's `input$file`).
#' @param max_size Maximum file size in bytes. Default is Inf.
#' @param allowed_types A vector of allowed MIME types. Default is NULL (all types allowed).
#' @param message Custom error message. If NULL, default messages are used.
#' @return A list with `is_valid` (logical) and `message` (character).
#' @export
#' @examples
#' # In a Shiny app, use file_validator(input$file)
file_validator <- function(file, max_size = Inf, allowed_types = NULL, message = NULL) {
  is_valid <- TRUE
  messages <- c()

  if (is.null(file) || nrow(file) == 0) {
    is_valid <- FALSE
    default_message <- "No file uploaded."
    messages <- c(messages, message %||% default_message)
  } else {
    if (file$size > max_size) {
      is_valid <- FALSE
      default_message <- glue("The file '{file$name}' exceeds the maximum size of {max_size} bytes.")
      messages <- c(messages, message %||% default_message)
    }
    if (!is.null(allowed_types) && !file$type %in% allowed_types) {
      is_valid <- FALSE
      default_message <- glue("The file '{file$name}' is not an allowed type.")
      messages <- c(messages, message %||% default_message)
    }
  }

  message <- paste(messages, collapse = " ")
  list(is_valid = is_valid, message = message)
}
