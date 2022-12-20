
pairwise_level <- function(list1, list2, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # Create empty vector to store results
  results <- vector()

  # Loop through all pairs of elements in the lists
  for (i in 1:length(list1)) {
    for (j in 1:length(list2)) {

      # Calculate the ratio level for the current pair of elements
      ratio_level <- unique_value_count(list1[[i]], list2[[j]], proportions=proportions, remove_na=ignore_na)

      # Add the calculated ratio level to the results vector
      results <- c(results, ratio_level)
    }
  }

  # If the 'names' argument is set to TRUE, add names to the results
  if (isTRUE(names)) {
    names(results) <- do.call(paste, c(as.list(1:length(list1)), as.list(1:length(list2))))
  }

  # Return the calculated ratio levels
  return(results)

}

