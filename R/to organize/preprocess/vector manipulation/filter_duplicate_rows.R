# TODO: Use `filter_duplicate_rows` inside of `filter_sequences()`

# Function to filter out duplicate rows in a data frame
filter_duplicate_rows <- function(df) {
  # Filter out duplicate rows in the input data frame
  df <- df[!duplicated(df), ]
  # Return the resulting data frame
  return(df)
}
