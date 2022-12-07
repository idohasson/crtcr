# Function factory that groups values according to their identifiers
group_by_id <- function(values, ids) {
  # Group values according to their identifiers
  grouped_values <- split(values, ids)

  # Return function that applies a quantification function to each group of values
  function(quantification_func) {
    # Apply quantification function to each group of values
    quantified_values <- lapply(grouped_values, quantification_func)

    # Return function that applies a final calculation function to the output of the quantification function
    function(calculation_func) {
      # Apply calculation function to the output of the quantification function
      calculated_values <- calculation_func(quantified_values)

      # Return the calculated values
      return(calculated_values)
    }
  }
}

# Example usage:
# In this example, the group_by_id function factory takes in the values and ids as input, and returns a function that applies the quantification_func to each group of values. This second function then returns a third function that applies the calculation_func to the output of the quantification_func. The final result is a list of calculated values that match the input identifiers.

# Define sample data
values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
ids <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)

# Define quantification function
quantification_func <- function(x) {
  return(mean(x))
}

# Define calculation function
calculation_func <- function(x) {

  return(sum(unlist(x)))
}

# Use function factory to group values by their identifiers, apply a quantification function, and apply a calculation function
result <- group_by_id(values, ids)(quantification_func)(calculation_func)

share_level2 <- function(.clone, .rid, .share_func=group_by_id, unique_func=quantification_func, func2=calculation_func) {

  .share_func(.clone, .rid)(unique_func)(func2)

  # .share_func()

  # function(func) {
  #
  #   func(x, ...)
  #
  #
  # }


}

f1 <- group_by_id(clone, rid)
f1(quantification_func)


# Example 2
f1 <- share_level2(id)

f1(.clone = clone)

clone <- rand_nt_vec(100)
cid <- vec_group_id(translate(clone))
rid <- rep(ids, 10)



# Function factory that groups values according to their identifiers
group_by_id <- function(values, ids) {
  # Group values according to their identifiers
  grouped_values <- split(values, ids)

  # Return function that applies a quantification function to each group of values
  function(quantification_func) {
    # Apply quantification function to each group of values
    quantified_values <- lapply(grouped_values, quantification_func)

    # Return function that applies a final calculation function to the output of the quantification function
    function(calculation_func) {
      # Apply calculation function to the output of the quantification function
      calculated_values <- calculation_func(quantified_values)

      # Return the calculated values
      return(calculated_values)
    }
  }
}
