# library(vctrs)
# library(dplyr)
# library(tibble)
# library(tidyr)

#' Check if a value appears more than once in a vector
#'
#' This function checks if a value appears more than once in a vector and returns
#' a logical vector indicating if a value appears more than once. This can be useful for
#' identifying convergent recombination events in T cell receptor (TCR) clonotypes.
#'
#' @param clone A vector.
#' @param ... Additional vectors.
#'
#' @return A logical vector indicating if a value appears more than once.
#'
#' @examples
#' is_cr(c("ACGT", "ACGT", "AACG", "ACGT"))
#' is_cr(c("ACGT", "ACGT", "AACG", "ACGT"), c("MKQ", "MKQ", "MKK", "MKQ"))
#' is_cr(c("ACGT", "ACGT", "AACG", "ACGT"), c("MKQ", "MKQ", "MKK", "MKQ"), c("group1", "group1", "group2", "group1"))
#'
#' @export
#'
#' @import vctrs
#' @importFrom vctrs df_list new_data_frame vec_duplicate_detect
is_cr <- function(clone, ...) {
  UseMethod("is_cr")
}

is_cr.default <- function(clone, ...) {
  # collect all input vector to a list
  dfl <- vctrs::df_list(clone, ..., .name_repair = "minimal")
  # create data frame to be compared by rows for any additional vector
  data <- vctrs::new_data_frame(dfl)
  # create a logical vector indicating if a value appears more than once
  vctrs::vec_duplicate_detect(data)
}

is_cr.character <- function(clone, ..., clonotype=translate(clone)) {
  # collect all input vector to a list
  dfl <- vctrs::df_list(clone, clonotype, ..., .name_repair = "minimal")
  # create data frame to be compared by rows for any additional vector
  data <- vctrs::new_data_frame(dfl)
  # create a logical vector indicating if a value appears more than once
  vctrs::vec_duplicate_detect(data)
}

