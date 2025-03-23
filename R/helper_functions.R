#' NULL coalescing operator
#'
#' @param a First value
#' @param b Default value if a is NULL
#' @return a if not NULL, otherwise b
#' @export
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Count regex pattern matches
#'
#' @param pattern Regular expression pattern
#' @param value String to search in
#' @return Number of matches found
#' @keywords internal
count_matches <- function(pattern, value) {
  matches <- gregexpr(pattern, value, perl = TRUE)[[1]]
  if (identical(matches, -1L)) {
    return(0)
  } else {
    return(length(matches))
  }
}
