#' Is public clonotype
#'
#' @param .id repertoire identifier vector
#' @param ... additional identifier vectors
#' @param .public minimum number of repertoires.
#'
#' @return TRUE for public, FALSE for private.
#' @export
#'
#' @examples
#'
#' is_public(c("A", "A", "A", "A"))
#' is_public(c("A", "A", "A", "A", "B"))
#'
is_public <- function(.id, .public=2) {
  unique_n(.id) >= .public
}
# vec_locate_matches


is_public <- function(v, ..., min_count=2,
                      public_func=NULL, public_condition=NULL) {

  v_val <- if (is.null(public_func)) n_unique(v) else public_func(v)

  is_p <- if (is.null(public_condition)) v_val >= min_count else public_condition(v_val)

  is_p

}

is_exclusive <- function(v, ..., max_count=1,
                         exclusive_func=NULL, exclusive_condition=NULL) {

  v_val <- if (is.null(exclusive_func)) n_unique(v) else exclusive_func(v)

  is_e <- if (is.null(exclusive_condition)) v_val <= max_count else exclusive_condition(v_val)

  is_e

}
