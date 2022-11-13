#' logical check if a frequencies vector is considered public-exclusive with optional modifications
#'
#' @param shared numerical vector
#' @param min_shared minimal value to be counted as repertoire having a clonotype
#' @param min_public minimal number of repertoire having a clonotype to be conciser public
#' @param max_exclusive upper limit to number of groups the could be considered exclusive
#'
#' @return TRUE for exclusive, FALSE for inclusive, and NA if non of the groups are public
#' @export
#'
#' @examples
#'
#' is_exclusive(c(0,0,0,3)) # TRUE (exclusive public)
#'
#' is_exclusive(c(0,1,2,3)) # FALSE (inclusive public)
#'
#' is_exclusive(c(0,1,2,3), min_shared=3) # TRUE (exclusive public)
#'
#' is_exclusive(c(0,1,2,3), min_shared=4) # NA (not public)
#'
#' is_exclusive(c(0,1,2,3), min_public=7) # NA (not public)
#'
#' is_exclusive(c(0,1,2,3), max_exclusive=3) # TRUE (exclusive public - up to three groups)
#'
#' is_exclusive(c(0,1,2,3), max_exclusive=2) # FALSE (inclusive public - more than two groups)
#'
#' is_exclusive(c(0,1,2,3), min_shared=2, max_exclusive=2) # TRUE (exclusive public - up to two groups)
#'
is_exclusive <- function(shared, min_shared=1L, min_public=1L, max_exclusive=1L) {

  public_shared <- is_public(shared, min_shared, min_public)

  if (!isTRUE(public_shared)) return(NA)

  sum(shared >= min_shared, na.rm = TRUE) <= max_exclusive

}
