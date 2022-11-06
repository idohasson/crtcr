#' Sub-classification of public clonotype
#'
#' @param public_min
#' @param exclusive_min
#' @param share_tbl
#'
#' @return
#'
#' @importFrom dplyr case_when
#'
#' @export
#'
#' @examples
cr_class <- function(share, public_min=1L, exclusive_min=1L) { # vector
  # TODO: check input and generate sharing table if needed
  # if (max(n, na.rm=TRUE) > public_min) return("private") else
  # if (sum(n!=0, na.rm=TRUE) > exclusive_min)

  cr_index <- function(n) {
  #           numeric vector of the unique number of samples
  #            having a specific clonotype in every group
  (max(n, na.rm=TRUE) > public_min) + (sum(n!=0, na.rm=TRUE) > exclusive_min)
  #       can't be private                        multiple shared samples
  #       public clonotype                          inclusive clonotype
  }
  apply(share_tbl, 1, cr_index) %>%
  # private = 0 | exclusive = 1 | inclusive = 2
  {case_when(. == 0 ~ "private",
            . == 1 ~ "exclusive",
            . == 2 ~ "inclusive")}
}
