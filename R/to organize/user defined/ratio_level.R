

ratio_level <- function(values1, values2, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # Check if the 'values1' and 'values2' vectors have the same length
  if (length(values1) != length(values2)) {
    stop("The 'values1' and 'values2' vectors must have the same length.")
  }

  # Calculate the number of unique values in each sequence
  values1_counts <- unique_value_count(values1, proportions=proportions, remove_na=ignore_na)
  values2_counts <- unique_value_count(values2, proportions=proportions, remove_na=ignore_na)

  # Calculate the ratio level
  ratio_level <- values1_counts / values2_counts

  # If the 'names' argument is set to TRUE, add names to the results
  if (isTRUE(names)) {
    names(ratio_level) <- c("values1", "values2")
  }

  # Return the calculated ratio level
  return(ratio_level)

}
