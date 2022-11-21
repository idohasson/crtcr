#' Add convergent recombination level to each clone record
#'
#' @description
#' Calculate unique number of nucleotide sequences encoding a specific amino
#' acid sequence. This value associated with TCR repertoire clonal sequences
#' and termed as "CR-level" (convergent recombination level). (CR-level)
#'
#' @param .data data frame which of the TCR repertoire's clone records. Each
#' row of the resulting data frame contains information about a single TCR
#' component sequence in one cell of a particular CDR3 sequence.
#' @param .clonal_seq Column variable name of the CDR3's nucleotide sequence.
#' @param ... additional grouping column variables that might be necessary to
#' distinguish merged data frames of multiple repertoires, or clonal sequence
#' specification (such as V/J gene segments).
#'
#' @return data frame with `CR_level` column
#'
#' @export
#'
#' @details
#' several clones that encoded the same amino acid sequence were found to be
#' structurally distinct at the nucleotide level, strongly implying clonal
#' selection and expansion is operating at the level of specific TCR-peptide
#' interactions.
#'
#' @examples
#'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#'
#' )
#'
#' add_cr_level(df, nt)
#' add_cr_level(df, nt, sample_id)
#'
#' require('dplyr')
#'
#' group_by(df, group_id) %>% add_cr_level(nt)
#' group_by(df, group_id) %>% add_cr_level(nt, sample_id)
#'
add_cr_level <- function(.data, .clonal_seq, ...) {

  .data %>%

    group_by(.cr=translate({{.clonal_seq}}), ..., .add = TRUE) %>%

    add_tally(wt = cr_level({{.clonal_seq}}), name = 'CR_level') %>%

    ungroup('.cr') %>% select(!'.cr')
}
