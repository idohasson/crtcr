#' logical check if a frequencies vector is considered public with optional modifications
#'
#' @param shared numerical vector
#' @param min_shared minimal value to be counted as repertoire having a clonotype
#' @param min_public minimal number of repertoire having a clonotype to be conciser public
#'
#' @return TRUE if its public, FALSE if private, and NA if none of the frequencies are sufficient
#' @export
#'
#' @examples
#'
#' is_public(c(0,1,0,0)) # FALSE (private clonotype)
#'
#' is_public(c(0,2,0,0)) # FALSE (public clonotype)
#'
#' is_public(c(0,2,0,0), min_public = 3) # FALSE (private clonotype)
#'
#' is_public(c(0,1,2,3), min_public = 5) # TRUE (public clonotype)
#'
#' is_public(c(0,2,0,0), min_shared=3) # NA (value is not sufficient)
#'
#' is_public(c(0,1,2,3), min_shared=3) # TRUE (public clonotype)
#'
#' is_public(c(0,1,2,3), min_shared=3, min_public = 5) # FALSE (private clonotype)
#'
is_public <- function(shared, min_shared=1L, min_public=2L) {

  total_shared <- sum(shared[shared >= min_shared], na.rm = TRUE)

  if (total_shared==0L) return(NA)

  total_shared >= min_public

}
