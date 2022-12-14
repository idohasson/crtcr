clonotype_data_frame <- function(nucleotide_seq, ...) {

  df <- bind_vectors(nucleotide_seq, ...)

  return(df)

}

# Function to join two character vectors into a data frame
repertoire_data_frame <- function(nucleotide_seq, aa_seq, ...) {
  # Create a data frame by combining the nucleotide and amino acid sequences
  df <- bind_vectors(nucleotide_seq, aa_seq, ...)

  # Return the resulting data frame
  return(df)
}

# Define the repertoire_group_data_frame() function
bind_vectors <- function(nucleotide_seq, ...) {

  # Bind the rows of the input data frames together
  dplyr::bind_rows(clone=nucleotide_seq, ...)
}


# Define the repertoire_group_data_frame() function
repertoire_group_data_frame <- function(df, ..., rep_id_name="rid") {
  # Bind the rows of the input data frames together
  bind_data_frames(df, ..., id_name = rep_id_name)
}


# Define the repertoire_group_data_frame() function
bind_data_frames <- function(df, ..., id_name="id") {
  # Bind the rows of the input data frames together
  dplyr::bind_rows(df, ..., .id = id_name)
}


# Function to filter out duplicate rows in a data frame
filter_duplicate_rows <- function(df) {
  # Filter out duplicate rows in the input data frame
  df <- df[!duplicated(df), ]

  # Return the resulting data frame
  return(df)
}
