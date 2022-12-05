scale_cr <- function(.clone, .id, .func) {

  (i <- vec_group_loc(xx)$loc)

  (g <- vec_chop(yy, i))

  (n <- lapply(g, vec_unique_count))

  (u <- vec_count(xx, sort = "location")$count)

  scale(n, u)



  indices <- vec_group_loc(.clone)$loc

  weights <- vec_count(xx, sort = "location")$count

  vec_chop(values, indices)

}


dimnames(A) <- list(fee=1:4, fie=1:3, fum=1:2)

mn_fum_fie <- apply(A, c("fum", "fie"), mean)
mn_fum_fie

sweep(A, c("fum", "fie"), mn_fum_fie)








  #' Scale values by a vector of weights
  #'
  #' This function scales the values in a numeric vector by the weights provided in a second vector. The resulting vector will have the same length as the input vector, but the values will be scaled according to the weights provided.
  #'
  #' @param values A numeric vector of values to be scaled
  #' @param weights A numeric vector of weights to use for scaling the values
  #' @return A numeric vector of scaled values
  #' @export
  scale_by_weights <- function(values, weights) {
    # Scale the values by the weights
    return(scale(values, weights))
  }


#' Scale values by weights computed by a function
#'
#' This function scales the values in a numeric vector by weights that are computed using a provided function. The function takes two values and a function as inputs, and uses the function to compute the weights to use for scaling the values. The resulting vector will have the same length as the input vector, but the values will be scaled according to the weights computed by the provided function.
#'
#' @param values A numeric vector of values to be scaled
#' @param value1 The first value to be passed to the weight-computing function
#' @param value2 The second value to be passed to the weight-computing function
#' @param weight_fn The function to use for computing the weights to use for scaling the values
#' @return A numeric vector of scaled values
#' @export
scale_by_computed_weights <- function(value1, value2, weight_fn) {
  # Compute the weights using the provided function
  weights <- weight_fn(value1)

  # Scale the values by the weights
  return(scale_by_weights(value2, weights))
}



#' Scale values by weights computed by a function
#'
#' This function scales the values in a numeric vector by weights that are computed using a provided function. The function takes two values and a function as inputs, and uses the function to compute the weights to use for scaling the values. The resulting vector will have the same length as the input vector, but the values will be scaled according to the weights computed by the provided function.
#'
#' @param values A numeric vector of values to be scaled
#' @param value1 The first value to be passed to the weight-computing function
#' @param value2 The second value to be passed to the weight-computing function
#' @param weight_fn The function to use for computing the weights to use for scaling the values
#' @return A numeric vector of scaled values
#' @export
freq_scale_by_computed_weights <- function(freq, value1, value2, weight_fn1, weight_fn2) {
  # Compute the weights using the provided function
  weights <- weight_fn(value1, value2, weight_fn1)

  # Scale the values by the weights
  return(scale_by_computed_weights(freq, weights, weight_fn2))
}

#' Compute a vector of values by ID using a given function
#'
#' This function computes a vector of values by ID using a provided function. The function takes a vector of IDs, a value to use as input for the provided function, and the function itself as inputs. It applies the function to each ID in the vector using the provided value as input, and returns the resulting vector of values.
#'
#' @param ids A vector of IDs to use as input for the provided function
#' @param value The value to use as input for the provided function
#' @param compute_fn The function to use for computing the values
#' @return A vector of values, computed by applying the provided function to each ID in the vector
#' @export
compute_values_by_id <- function(ids, value, compute_fn) {
  # Compute the values using the provided function
  return(sapply(ids, compute_fn, value))
}




