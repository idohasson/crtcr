impute_missing_values <- function(sequences, method) {
  # Fill in missing values in a TCR clonotype sequence dataset
  if (method == "mean") {
    # Mean imputation
    sequences[sequences == "-"] <- rowMeans(sequences, na.rm = TRUE)
  } else if (method == "median") {
    # Median imputation
    sequences[sequences == "-"] <- rowMedians(sequences, na.rm = TRUE)
  }
  return(sequences)
}
