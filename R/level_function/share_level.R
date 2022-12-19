clonotype_split <- function(aa, id, ...) {

  # id_list <- dots_splice(id, ...)
  dfl <- df_list(id, ..., .name_repair = "unique_quiet")

  df <- new_data_frame(dfl, class = "clonotype")

  clonotype_df <- vec_split(df, aa)

  tibble::deframe(clonotype_df)

}

# share_level(ID)
# share_level(clonotype_split(AA, ID))
share_level <- function(id, ...) {

  id_list <- dots_splice(id, ...)

  vapply(id_list, vec_unique_count, integer(1))

}
# group_share_level(clonotype_split(AA, ID))
group_share_level <- function(id_list, group_level=mean) {

  if (is_formula(group_level))

    group_level <- as_function(group_level)

  if (is_function(group_level)) {

    levels <- share_level(id_list)

    group_level <- group_level(levels)

  } else if (is_null(group_level)) {

    group_id <- unlist(id_list, use.names = FALSE)

    group_level <- share_level(group_id)

  } else stop("group_func must be either a function, formula or NULL")

  return(group_level)
}

combined_function <- function(data_list, level_func, group_level=NULL, ...) {

  # Calculate the number of unique values for each vector in the list
  unique_counts <- lapply(data_list, level_func, ...)

  # Calculate the level of shared values using the provided function
  shared_level <- level_func(unique_counts)

  # If the 'names' argument is set to TRUE, add names to the results
  if (is_null(group_level)) {

    return(shared_level)

  } else if (is_formula(group_level)) {

    group_level <- as_function(group_level)

  } else if (!is_function(group_level)) stop(

    "group_func must be either a function, formula or NULL"

  )

  group_level(levels)

}

unique_value_count <- function(value_vector, proportion=FALSE, remove_na = FALSE) {

  # Check if there are duplicated values in the input vector
  has_duplicates <- duplicated(value_vector)

  # If the 'remove_na' argument is set to TRUE, find the indices of non-NA values in the vector
  if (isTRUE(remove_na)) {
    non_na_indices <- vctrs::vec_detect_complete(value_vector)
    has_duplicates <- has_duplicates & non_na_indices
  }

  # Count the number of unique nucleotide sequences
  unique_count <- sum(!has_duplicates)

  # If the 'proportion' argument is set to TRUE, return the proportion of unique nucleotide sequences,
  # otherwise return the count
  if (isTRUE(proportion)) {
    return(unique_count / vec_size(value_vector))
  } else {
    return(unique_count)
  }

}

group_shared_level <- function(data_list, level_func, proportions=FALSE, ignore_na = FALSE, names=FALSE) {

  # Calculate the number of unique values for each vector in the list
  unique_counts <- lapply(data_list, unique_value_count, proportions=proportions, remove_na=ignore_na)

  # Calculate the level of shared values using the provided function
  shared_level <- level_func(unique_counts)

  # If the 'names' argument is set to TRUE, add names to the results
  if (isTRUE(names)) {
    names(shared_level) <- names(data_list)
  }

  # Return the calculated level of shared values
  return(shared_level)

}

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


