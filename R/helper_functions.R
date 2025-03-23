# NULL coalescing
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# Helper function to count matches
count_matches <- function(pattern, value) {
  matches <- gregexpr(pattern, value, perl = TRUE)[[1]]
  if (identical(matches, -1L)) {
    return(0)
  } else {
    return(length(matches))
  }
}

