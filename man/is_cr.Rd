% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_cr.R
\name{is_cr}
\alias{is_cr}
\title{Check if a value appears more than once in a vector}
\usage{
is_cr(nt)
}
\arguments{
\item{clone}{A vector.}

\item{...}{Additional vectors.}
}
\value{
A logical vector indicating if a value appears more than once.
}
\description{
This function checks if a value appears more than once in a vector and returns
a logical vector indicating if a value appears more than once. This can be useful for
identifying convergent recombination events in T cell receptor (TCR) clonotypes.
}
\examples{
is_cr(c("ACGT", "ACGT", "AACG", "ACGT"))
is_cr(c("ACGT", "ACGT", "AACG", "ACGT"), c("MKQ", "MKQ", "MKK", "MKQ"))
is_cr(c("ACGT", "ACGT", "AACG", "ACGT"), c("MKQ", "MKQ", "MKK", "MKQ"), c("group1", "group1", "group2", "group1"))

}
