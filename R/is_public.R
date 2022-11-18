#' Public clonotype frequencies
#'
#' check if clonotype's frequencies throughout repertoire group are public
#'
#' @param freq  Numerical vector.
#' @param .public Minimum number of repertoires
#'
#' @return logical scalar. TRUE for public, FALSE for private (or none)
#' @export
#'
#' @examples
#'
#' is_public(c(0,1,0,3))
#'
is_public <- function(freq, .public=2) {

  # sum(as.logical(freq), na.rm = TRUE) > .public
  # N <- sum(freq  >= .min, na.rm = TRUE)

  N <- sum(as.logical(freq), na.rm = TRUE)

  if (N == 0)

    return(NA)

  if (.public <= 1)

    N <- N / length(freq)

  N >= .public

}







# is_public
# Input:
#   * freq - numerical vector
#   * .min - numerical scalar (default 1).
#             * .min > 0: filter out [value] < .min
#             * .min <= 0: filter out [value] >= .min
#   * .public - numerical vector (default 2):
#             * .public > 1: TRUE if #[values > .min] >= .public
#             * 0 < .public <= 1: TRUE if [values > .min]%  >= .public
#   * rm.na - if FALSE return (default) return NA if #[values >/<= .min] = 0
#           - if TRUE return FALSE for those cases
# Output:
#   * logical scalar length 1

#   * .relative - logical
#             * apply function so sum([values])=1


# is character vector public
# Input:
#   * character vector
# Output:
#   *


# is list (df) public
# Input:
#   *
# Output:

# is numeric public
# Input:
#   *
# Output:
