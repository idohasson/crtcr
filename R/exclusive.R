#' Is exclusive public clonotype
#'
#' True for if clonotype’s present across subgroups doesn't surpass the threshold
#'
#' @param .subgroup sub-group identification vector.
#' @param ... additional identifier vectors
#' @param .exclusive maximal number of subgroups a clone is found in.
#' @param .is_public logical value of of wheather or not the clonotype is public. if FALSE, meaning it's private, return NA.
#' @param na.rm return `FALSE` instead of `NA` when `.is_public` is FALSE
#'
#' @return TRUE for exclusive, FALSE for inclusive.
#' @export
#'
#' @examples
#'
#' is_exclusive(c("A", "A", "A", "A"))
#' is_exclusive(c("A", "A", "A", "A", "B"))
#'
is_exclusive <- function(.subgroup, .exclusive=1, .is_public=NULL, na.rm=FALSE) {

  # TODO: check public. (like in `is_exclusive_freq`)

  if (isFALSE(.is_public)) {

    if (na.rm)

      return(FALSE)

    else

      return(NA)

  }

  vec_unique_count(.subgroup) <= .exclusive

}

# with(df, share_level(id, aa, id2))
# with(df, share_level(id2, aa, id))
# share_level(id, aa)
# vec_match(c("A", "A", "A", "A"))
