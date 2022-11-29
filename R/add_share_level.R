#' Add share level of clonotypes among repertoires
#'
#' @param .data repertoire data frame
#' @param .clonotype clonotype sequence variable
#' @param .rep_id repertoire identification column
#' @param ... additional variables to group by
#'
#' @return data frame with 'share' column
#' @export
#'
#' @details
#' Previous studies have shown that the extent of sharing and
#' the clonotypic frequency of TCRb sequences are significantly
#' correlated with their production efficiencies in simulations
#' of a random recombination process because of the phenomenon
#' of convergent recombination.
#' https://doi.org/10.1073/pnas.1319389111 (REMOVE) ###########################################
#'
#' @examples
#'
#' df <- data.frame(
#'   group_id=gl(2,4,8,labels = c('cancer','control')),
#'   sample_id=gl(4,2,8,labels = LETTERS[1:4]),
#'   nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
#'        'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
#'   aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
#' )
#'
#' add_share_level(df, aa, sample_id)
#'
#' require('dplyr')
#'
#' group_by(df, group_id) %>% add_share_level(aa, sample_id)
#'
add_share_level <- function(.data, .clonotype, .rep_id, ...) {

  .data %>%

    group_by({{.clonotype}}, ..., .add = TRUE) %>%

    add_tally(wt = share_level({{.rep_id}}), name = "share")

}
