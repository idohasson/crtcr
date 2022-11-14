#' is clonotype public by sub-groups share-level
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
is_public <- function(shared, min_shared=1, min_public=2*min_shared) {

  in_sub_gruop <- shared >= min_shared

  if (!any(in_sub_gruop, na.rm = FALSE)) return(NA)

  sum(shared[in_sub_gruop], na.rm = TRUE) >= min_public

  # total_shared <- sum(shared[shared >= min_shared], na.rm = TRUE)

  # if (total_shared==0) return(NA)

  # total_shared >= min_public

}




# is_public(1,0,0)
# is_public(c(0,2,0))
# is_public(c(0,0,0))
# is_public(c(.1,0,0))
# is_public(.05,0,0, min_public = .1)
# is_public(c(.1,0,0), min_public = .5)
# is_public(c(.1,.1,0), min_public = .5)
# is_public(c(.2,0,0), min_public = .5)
# is_public(c(.2,.2,0), min_public = 2)
# is_public(c(.2,.2,.1), min_public = 1)


is_public <- function(..., min_public=2) {

  in_rep <- c(...)>0

  in_group <- sum(in_rep, na.rm = TRUE)

  if (in_rep == 0)
    return(NA)

  if (min_public <= 1)
    in_group <- in_group / length(in_rep)

  in_group >= min_public

}


# rbind()


l <- list(LETTERS[1:3], LETTERS[3:5], LETTERS[3], LETTERS[2:6])


purrr::map_dfr(l, table)


is_public_list <- function(..., min_public=2) {

  rep_list <- rlang::dots_splice(...)

  rep_list <- rlang::squash(rep_list)

  rep_list <- rlang::squash(rep_list)

  tbl <- purrr::map_dfr(rep_list, table)

  as.data.frame(t(tbl))

}











