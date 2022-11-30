#' CR-level for multiple repertoires
#'
#' @param .clone
#' @param .id
#' @param .subgroup_func
#' @param .subgroup
#'
#' @return
#' @export
#'
#' @examples
cr_subgroup <- function(.clone, .id, .subgroup, .subgroup_func=mean) {

  cr_df <- across_unique(.clone, .id, .sub=.subgroup, apply_func = cr_level)

  tapply(cr_df[["cr_level"]], cr_df[[".sub"]], .subgroup_func)

}

