
# Function to combine multiple data frames provided by the user
combine_user_data <- function(user_data, delimiter = ",") {

  # # Define a function to calculate the shared convergent recombination sequences
  # calculate_shared_sequences <- function(sequences) {
  #   # Code to calculate the shared sequences goes here
  #   vctrs::vec_unique_count(sequences)
  # }
  #
  # # Use the map function to apply the calculate_shared_sequences function to each element of the nucleotide_sequences column
  # clonotype_data <- x %>% split(.$id) %>% combine_user_data
  # purrr::map(clonotype_data$nucleotide_sequences, calculate_shared_sequences)
  #

  # Convert the user-provided data frames to data frames, if necessary
  user_data_frames <- lapply(user_data, data.frame)

  # Bind the rows of the user-provided data frames together into a single data frame
  sequencing_data <- dplyr::bind_rows(user_data_frames, .id = ".rid")

  # Group the data by amino acid sequence and summarize the nucleotide sequences for each group
  clonotype_data <- sequencing_data %>%
    dplyr::group_by(aa) %>%
    dplyr::summarize(nucleotide_sequences = list(clone=nt, id=.rid))

  # Return the resulting data frame
  return(clonotype_data)
}

library(purrr)



preprocess_data <- function(datasets, column, min_length, max_length, min_count, max_count, ...) {
  # Preprocess multiple datasets containing TCR clonotype sequences
  datasets <- lapply(datasets, function(data) {

    # Extract the sequences from the specified column
    if (is.data.frame(data)) {
      sequences <- data[[column]]
    } else if (is.list(data)) {
      sequences <- unlist(column)
    } else {
      sequences <- data
    }

    # Clean and standardize the sequences
    sequences <- clean_sequences(sequences)
    sequences <- standardize_format(sequences, "nucleotide")

    # Remove duplicate sequences
    sequences <- remove_duplicates(sequences)

    # Remove partial sequences and filter based on length and count
    sequences <- remove_partial_sequences(sequences, min_length)
    sequences <- filter_sequences(sequences, min_length, max_length, min_count, max_count, ...)

    # Impute missing values and normalize sequences to a common reference
    # sequences <- impute_missing_values(sequences, "mean")
    # sequences <- normalize_sequences(sequences, "align", reference = "ATGCAGTAGCAGTAGCAGTAGCAGTAGC")

    return(sequences)

  })
  # datasets
  # Merge the preprocessed datasets into a single dataset
  # sequences <- merge_datasets(datasets, by = "row")
  datasets
  # # Calculate the shared level of convergent recombination sequences among repertoires
  # shared_level <- colMeans(sequences == "ATGCAGTAGCAGTAGCAGTAGCAGTAGC")
  #
  # return(list(sequences = sequences, shared_level = shared_level))
  # return(sequences)
}

#### string manipulation #####

clean_sequences <- function(sequences) {
  # Remove any non-alphabetic characters and convert to uppercase
  sequences <- gsub("[^[:alpha:]]", "", sequences, perl = TRUE)
  sequences <- toupper(sequences)
  return(sequences)
}

standardize_format <- function(sequences, format) {
  # Ensure that all sequences are in the same format
  if (format == "nucleotide") {
    sequences <- gsub("[^ATGC]", "-", sequences, perl = TRUE)
  } else if (format == "amino_acid") {
    sequences <- gsub("[^ARNDCQEGHILKMFPSTWYV]", "-", sequences, perl = TRUE)
  }
  return(sequences)
}

normalize_sequences <- function(sequences, method, reference) {
  # Normalize sequences to specified length or reference sequence
  if (method == "trim") {
    # Trim sequences to specified length
    sequences <- trim_sequences(sequences, length)
  } else if (method == "pad") {
    # Pad sequences to specified length
    sequences <- paste0(sequences, rep("-", length - nchar(sequences)))
  } else if (method == "align") {
    # Align sequences to reference sequence
    sequences <- vmatchPattern(sequences, reference)
  }
  return(sequences)
}

trim_sequences <- function(sequences, length) {
  # Trim sequences to specified length
  sequences <- substr(sequences, 1, length)
  return(sequences)
}


#### Sequence filtration #####

remove_duplicates <- function(sequences) {
  # Remove duplicate sequences
  sequences <- unique(sequences)
  return(sequences)
}

filter_sequences <- function(sequences, min_length, max_length, min_count, max_count, ...) {
  # Filter sequences based on user-specified criteria
  sequences <- sequences[nchar(sequences) >= min_length & nchar(sequences) <= max_length]
  sequences <- sequences[table(sequences) >= min_count & table(sequences) <= max_count]
  return(sequences)
}

# TODO: Use `filter_duplicate_rows` inside of `filter_sequences()`

# Function to filter out duplicate rows in a data frame
filter_duplicate_rows <- function(df) {
  # Filter out duplicate rows in the input data frame
  df <- df[!duplicated(df), ]
  # Return the resulting data frame
  return(df)
}

remove_partial_sequences <- function(sequences, min_length) {
  # Remove sequences that are shorter than specified length or have gaps within the sequence
  sequences <- sequences[nchar(sequences) >= min_length & !grepl("-", sequences)]
  return(sequences)
}

#### Data manipulation #####

merge_datasets <- function(..., by = "row") {
  # Merge multiple datasets containing TCR clonotype sequences
  datasets <- lapply(..., as.data.frame)
  # dfl <- lapply(..., as.data.frame)
  if (by == "row") {
    sequences <- dplyr::bind_rows(datasets, .id = ".rid")
    # sequences <- do.call(rbind, datasets)
  } else if (by == "column") {
    sequences <- do.call(cbind, datasets)
  }
  return(sequences)
}

# TODO: Use `bind_data_frames` inside of `merge_datasets()`
# Define the repertoire_group_data_frame() function
# bind_data_frames <- function(df, ..., id_name="id") {
#   # Bind the rows of the input data frames together
    # have_name()
#   # dplyr::bind_rows(df, ..., .id = id_name)
#   # vctrs::vec_rbind(..., .name_repair = "unique_quiet")
# }



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


