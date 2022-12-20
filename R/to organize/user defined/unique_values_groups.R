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
