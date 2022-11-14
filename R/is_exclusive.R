#' logical check if a frequencies vector is considered public-exclusive with optional modifications
#'
#' @param shared numerical vector
#' @param max_exclusive upper limit to number of groups the could be considered exclusive
#'
#' @return TRUE for exclusive, FALSE for inclusive, and NA if non of the groups are public
#' @export
#'
#' @examples
#'
#' is_exclusive(c(0,0,0,3)) # TRUE (exclusive public)
#'
is_exclusive <- function(gruop_freq, max_exclusive=1) {

  gruop_freq <- share_level(gruop_freq)

  if (gruop_freq == 0)

    return(NA)

  else if (max_exclusive >= 1)

    return(gruop_freq <= max_exclusive)

  else

    return(gruop_freq / length(gruop_freq) <= max_exclusive)

}



