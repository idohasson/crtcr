#   CR-level requires - at least one repertoire
#   Share-level requires - at least one group
#   CR-class requires - at least two groups
#   CR-ts (time series) requires - at least two groups
#   TODO CR-ts (?)
#
#
# valid CR-class - |GROUPS| > 1, |REPERTOIRES| > 0; NT|(NT+AA) per repertoire
#
#   nt (character vector) - clonal sequence is one of the unique collection of
#                           CDR3s coding sequences of a specific CDR3-peptide
#   aa (character vector) - clonotype is the CDR3-peptide sequence originated by
#                           either one of its clones
#
#   data type specification (either one of these choices)
#
#   character / factors
#   - a depth-two-list - list of list of NT-vectors. a group is considered as list of NT-vectors)
#
#   data.frame
#   - column order should be
#     1. gid (always first)
#     2. rid (only second to gid)
#     3. nt (always before aa or last for missing aa)
#     4. aa (always last) - is optional, and if not provided, calculated as last column out of NT column codon translation
#   - provide column name by gid, rid, clone_col, clonotype_col for any additional column
#
#   1. list of list of data.frames
#     - character vector
#     - factor
#     - two columns named 'nt' and 'aa'
#     - 1 column NT
#   2. list of data.frames
#     - a column named 'gid' + a column named 'nt'
#     - a column named 'gid' + two column named 'nt' and 'aa'
#   3.data.frame
#     - two columns named 'gid' and 'rid' + a column 'nt'
#     - two columns named 'gid' and 'rid' + two columns named 'nt' and 'aa'

AA_FIELDS <- c("amino_acid", "aaSeqCDR3", "cdr3aa", "CDR3 amino acid sequence",
               "CDR3.amino.acid.sequence", "CDR3aa", "junction_aa",
               "CDR3.aa", "AAseq", "Amino acid sequence", "cdrAASeq")

NT_FIELDS <- c("rearrangement", "nSeqCDR3", "CDR3 nucleotide sequence",
               "CDR3.nucleotide.sequence", "cdr3nt", "junction", "cdr3_nt",
               "CDR3.nt", "NNseq", "Junction nucleotide sequence", "cdrNucSeq")



#' Join all repertoires' clonotypes data into one data frame
#'
#' @param ... repertoire group
#'
#' @return data frame
#'
#' @importFrom rlang list2
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr modify_depth map pluck
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'
#' @examples
#'
#' # generate random
#' g <- replicate(4, rand_group(), simplify = FALSE)
#'
#' group_join(g)
#'
group_join <- function(...) {

  rep <- list2(...) # TODO: dots_list
  # TODO check valid input
  # TODO make sure rep values are unique valid input
  if (is.vector(rep)) {

    if (is.vector(pluck(rep, 1))) {

      if (is.list(rep) & length(rep)==1)
        rep %<>% pluck(1)

      if (is.character(pluck(rep, 1, 1)))
        rep %<>% modify_depth(2, function(nt)
          cbind.data.frame(clone=nt,
                           clonotype=translate(nt)))

      if (is.data.frame(pluck(rep, 1, 1)))
        rep %<>% map(bind_rows, .id = "rep_id")
    }

    if(is.data.frame(pluck(rep, 1)))
      rep %<>% bind_rows(.id = "group")

  } else if (is.data.frame(rep)) {

    if (is.character(rep[1,3]) & ncol(rep)==3)
      rep %<>% mutate(clonotype=translate(pluck(3)))

  } else stop("Invalid input error")

  # # TODO check valid input
  # if (ncol(rep) == 4)
  #   colnames(rep) <- c("group", "rep_id", "clone", "clonotype")
  # else stop("ERROR")

 rep

}
