split_dataset <- function(sequences, method, ...) {
  # Split a dataset containing TCR clonotype sequences into two or more subsets
  if (method == "random") {
    # Random sampling
    indices <- sample(nrow(sequences), ...)
    subsets <- lapply(indices, function(i) sequences[i,])
  } else if (method == "stratified") {
    # Stratified sampling
    indices <- unlist(lapply(..., function(x) which(sequences == x)))
    subsets <- lapply(indices, function(i) sequences[i,])
  }
  return(subsets)
}
