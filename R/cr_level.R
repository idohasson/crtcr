cr_level <- function(...) {
  UseMethod("cr_level")
}

#' Title
#'
#' @param clonal_seq
#' @param check_clonotype
#'
#' @return
#' @export
#'
#' @examples
cr_level.character <- function(clonal_seq, ...) {

  vec_unique_count(clonal_seq)

}

#' Title
#'
#' @param clonal_seq
#'
#' @return
#' @export
#'
#' @examples
cr_level.list <- function(...) {



  list_all_size(clonal_seq)

  clonal_seq

  # list_all_size
}

