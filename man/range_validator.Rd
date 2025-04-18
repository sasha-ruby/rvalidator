% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validator_range.R
\name{range_validator}
\alias{range_validator}
\title{Range Validator}
\usage{
range_validator(value, min = NULL, max = NULL, message = NULL)
}
\arguments{
\item{value}{The value to validate.}

\item{min}{The minimum value.}

\item{max}{The maximum value.}

\item{message}{Custom error message. If NULL, default messages are used.}
}
\value{
A list with \code{is_valid} (logical) and \code{message} (character).
}
\description{
Validates that a numeric value falls within a specified range.
}
\examples{
range_validator(5, min = 1, max = 10)
range_validator(15, max = 10)
}
