
#' logical check if a frequencies vector is considered public with optional modifications
#'
#' @param freq numerical vector
#' @param min_freq minimal value to be counted as repertoire having a clonotype
#' @param min_rep minimal number of repertoire having a clonotype to be conciser public
#'
#' @return TRUE if its public, FALSE if private, and NA if none of the frequencies are sufficient
#' @export
#'
#' @examples
#'
#' is_public(c(1, 0 , 2, 4), min_freq=3)
#'
is_public <- function(freq, min_freq=1L, min_rep=2L) {
  # TODO check changes
  # if (all(freq==0, na.rm = TRUE)) return(NA)
  if (all(freq >= min_freq, na.rm = TRUE)) return(NA)

  sum(freq >= min_freq, na.rm = TRUE) >= min_rep

}

#' logical check if a frequencies vector is considered public-exclusive with optional modifications
#'
#' @param freq freq >= min_freq
#' @param id same length vector to split the groups by
#' @param min_freq minimal value to be counted as repertoire having a clonotype
#' @param min_rep minimal number of repertoire having a clonotype to be conciser public
#' @param max_group upper limit to number of groups the could be considered exclusive
#'
#' @return TRUE for exclusive, FALSE for inclusive, and NA if non of the groups are public
#' @export
#'
#' @examples
#'
#' is_exclusive(c(1, 0 , 2, 4), gl(2,2), min_freq=2)
#'
is_exclusive <- function(freq, id, min_freq=1L, min_rep=1L, max_group=1L) {

  sub_public <- tapply(freq, id, is_public, min_freq, min_rep)

  if (all(is.na(sub_public))) return(NA)

  sum(sub_public, na.rm = TRUE) <= max_group

}
