#' Title
#'
#' @param df data frame which include the column of the repertoire's colonotype
#' as the CDR3s' amino acid sequences
#' @param nt_col Clonal sequence column name (optional)
#' @param aa_col Clonotype sequence column name (optional)
#' @param out_type
#'
#' @return
#' @export
#'
#' @examples
#'
get_clonotypes <- function(df, nt_col, aa_col, out_type="list") {

  # TODO: unique clonotype vector
  # TODO: add from_file option
  # check that the out_type argument is one of the three allowed options.
  out_type <- arg_match(out_type, c("list", "data.frame", "vector", "unique"))

  if (out_type == "unique") {

    # if (missing(aa_col) )

    clonotypes <- get_aa(df, aa_col)

    return(unique(clonotypes))

  }
  # extract the nucleotide sequences from the data frame.
  nt <- get_nt(df, nt_col)
  stopifnot(is.character(nt))
  # extract the amino acid sequences from the data frame.
  aa <- get_aa(df, aa_col)
  # check that the amino acid vector is the same length as the nucleotide vector.
  if (is.character(aa)) stopifnot(length(aa) == length(nt))
  # convert the nucleotide sequence to an amino acid sequence.
  else if (is.null(aa)) aa <- translate(nt)


  if (out_type == "list") {
    # splits a vector into a list of vectors.
    clonotypes <- split(nt, aa) %>%
      # return a list of unique clonal sequences (NT) named by their
      lapply(unique) # corresponding clonotype sequence(AA).

  } else if (out_type == "data.frame") {
    # binds columns together into a data frame.
    clonotypes <- cbind.data.frame(clone = nt, clonotype = aa) %>%
      # returns the unique rows of a data frame.
      distinct(clone, .keep_all = TRUE)

  } else if (out_type == "vector") { # vector
    # binds columns together into a data frame.
    clonotypes <- cbind.data.frame(nt, aa) %>%
      # returns clonotype sequences (AA) by identifying unique pairing to their
      vec_unique_loc %>% aa[.] # corresponding clonal sequence (NT).
  }

  clonotypes

}

#' Title
#'
#' @param df_list
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' rep_list <- rand_group("df_list", 3, seq_len=3)
#'
#' lapply(rep_list, get_clonotypes, nt_col="nt", aa_col="aa", out_type="data.frame")
#'
get_clonotypes_list2 <- function(df_list, ..., each_unique=FALSE) {
  # check that df_list is a list.
  stopifnot(is.list(df_list))
  # check that every element of df_list is a data frame.
  stopifnot(purrr::every(df_list, is.data.frame))
  # applies the get_clonotypes function to each element of df_list.
  if(!each_unique) return(lapply(df_list, get_clonotypes, ...))
  # It checks if the ellipsis_args dictionary has a key named f_arg.
  ellipsis_args <- list2(...); f_arg <- formalArgs(get_aa)[2]
  # If it does, it returns the value of that key. If it doesn't, it returns an empty list.
  arg_l <- if (has_name(ellipsis_args, f_arg)) ellipsis_args[f_arg] else list()
  # takes a list of data frames and returns a list of unique amino acid sequences.
  lapply(df_list, function(x) unique(do.call("get_aa", append(list(df=x), arg_l))))
}


#' Title
#'
#' @param df_list
#' @param indices
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_clonotypes_list <- function(df_list, to_df=FALSE, ..., indices) {
  # check that df_list is a list.
  stopifnot(is.list(df_list))
  # check that every element of df_list is a data frame.
  stopifnot(purrr::every(df_list, is.data.frame))
  # checks if the list is named, and if so, it sets the names of the data frames
  # to the sequence of integers starting from 1.
  if (!is_named(df_list)) names(df_list) <- as.character(seq_along(df_list))

  # df_list <- df_list[indices]

  if (!missing(indices)) {

    lapply(indices, function(i) get_clonotypes_list(df_list[i], to_df, ...)) %>%

      returnValue()

  }



  if (to_df) {

    df <- df_list %>%
    # Get the list of clonotypes data frames for each repertoire.
    get_clonotypes_list(out_type = "data.frame", ...) %>%
    # Bind the list of clonotypes for each sample into a single data frame.
    bind_rows(.id = "sample")

    return(df)

  }

  lapply(df_list, get_clonotypes, ...)

}

#' Title
#'
#' @param rep repertoires input data which is either one of:
#' - character vector of nucleotide sequences encoding to the CDR3 amino acid clonotype sequence
#' - character vector list
#' - data frame which include the column of the repertoire's colonotype as the CDR3s' amino acid sequences
#' - data frame list
#' @param aa_col if rep is a data frame, column name of clonotype column.
#'
#' the name of the amino acid column to extract the clonotypes from.
#' if the input data is a list, extract the same column name from all of them,
#' or altenativly a character vector with the same length of the list for each
#' of the corrisponding dataframes' column
#' @param distinct_clone_by if provided, distinct between clonotype duplicates
#' by additional column (or columns) for data frame input, or character vector
#' for character vector input.
#' for single data frame input, provide the additional column name as an objects of type "character" or
#' character vector for multiple columns. for list of dataframe input, provide
#' ether as previously mentioned applied to every data frame in the list, or rather
#' same size list of diffrent column names of the corrisponding data frame in the list
#' @param group_rep same length as the input data character or numerical vector
#' to group by the repertoire samples. same value indicates for same group. if the
#' input data is a data frame column name to be group by can be provided alternativly.
#'
#' @return character vector for each entry
#'
#' @export
#'
#' @examples
#'
#' get_clonotypes(aa_vec)
#' get_clonotypes(list_of_aa_vec)
#'
#' get_clonotypes(df)
#' get_clonotypes(df_list)
#'
#'




