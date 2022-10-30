

#' Title
#'
#' @param nt character vector or character vector list of the clonotypes' necleotide sequences
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
get_clonotype_from_nt <- function(nt) {

  if (is.list(nt)) {
    return(lapply(nt, get_clonotype_from_nt))
  }

  seq_translate(dna(nt))
}

rep$nSeqCDR3[1:10] %>%

  get_clonotype_from_nt

rep <- df
rep
# character vector
aa_col <- "aaSeqCDR3"
distinct_clone_by <- "nSeqCDR3"
distinct_clone_by <- c("nSeqCDR3", "targetQualities")

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
get_clonotypes <- function(..., aa_col, distinct_clone_by, group_rep) {

}

get_clonotypes.data.frame <- function(rep, aa_col, distinct_clone_by) {
  # if is a tabele
  if (is.data.frame(rep) | is.matrix(rep)) {
    # table has the column
    stopifnot(length(aa_col) == 1)
    # stopifnot(is.character(aa_col) | is.numeric(aa_col))
    stopifnot(is.character(aa_col) & has_name(rep, aa_col))
    # stopifnot(is.numeric(aa_col) & (ncol(rep) < aa_col))

    if (!missing(distinct_clone_by)) {

      stopifnot(all(has_name(x=rep, distinct_clone_by)))

      rep <- select_at(rep, union(aa_col, distinct_clone_by))
      # rep <- distinct_at(rep, union(aa_col, distinct_clone_by))
    }

    rep <- distinct(rep)

    return(pull(rep, aa_col))

  }
}


# if provided clone parameters
# if (!missing(distinct_clone_by)) {}
# else {}
# all(sapply(distinct_clone_by, has_name, x = rep))
distict_by(union(aa_col, distinct_clone_by))

if (is.character(rep)) {

  return(unique(rep))
}


is_named(rep)
