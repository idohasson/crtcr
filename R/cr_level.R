#' Convergent recombination level (CR-level) of clonal sequences
#'
#' @description
#' calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level).
#'
#' @param ... nucleotide sequences
#' @param always_named name single value. Default `TRUE`.
#'
#' @return CR-level integer vector (named by the clonotype)
#' @export
#' @examples
#' cr_level("ATGTTT", "ATCTTC", "ATGTTC", "AAA")
#'
cr_level <- function(..., always_named=FALSE) {

  clonal_sequence <- c(...)

  clonal_sequence %<>%

    split(translate(.)) %>%

    sapply(n_distinct)

  if (always_named | !testScalar(clonal_sequence))

    return(clonal_sequence)

  else

    return (unname(clonal_sequence))

}


#' Convergent Recombination level for data frame's clonal sequence column
#'
#' @description
#' calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level).
#'
#'
#' @param df data frame
#' @param ... Column names to group the clones by as distinct repertoires
#' @param clone_var Column name of CDR3's nucleotide sequence
#' @param clonotype_var (optional) Column name of CDR3's amino acid sequence
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGGGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTCAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#'
#' cr_level_df(df, clone_var=nt, clonotype_var=aa)
#'
#' cr_level_df(df, clone_var=nt)
#'
#' cr_level_df(df, group_id, sample_id, clone_var=nt, clonotype_var=aa)
#'
cr_level_df <- function(df, ..., clone_var, clonotype_var) {

  if (missingArg(clonotype_var)) {
    df <- mutate(df, clonotype=translate({{clone_var}}))
    clonotype_var <- as.name('clonotype')
  }

  group_by(df, {{clonotype_var}}, ..., .add = FALSE) %>%

    summarise(across({{clone_var}}, n_distinct, .names = "CR_level"), .groups = "drop")

}
