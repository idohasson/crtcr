

# is_exclusive_freq <- function(..., .exclusive=1) {
#
#   clonotype_data <- vctrs::df_list(..., .name_repair = "minimal")
#
#   clonotype_data <- vctrs::new_data_frame(clonotype_data)
#
#   freq <- vctrs::vec_unique_count(clonotype_data)
#
#   if (.exclusive >= 1)
#
#     return(freq <= .exclusive)
#
#   else {
#
#     n_levels <- sapply(seq_along(clonotype_data),
#                        function(i) length(levels(clonotype_data[,i])))
#
#     return(freq / sum(n_levels) <= .exclusive)
#
#   }
#
# }

#' Is exclusive public clonotype by frequencies
#'
#' @param .freq numerical vector
#' @param .subgroup identifier vector (optional)
#' @param .exclusive maximal number of subgroups a clone is found in.
#' @param na.rm if FALSE (the default), return NA when all frequency values are zero.
#'
#' @return TRUE for exclusive, FALSE for inclusive.
#' @export
#'
#' @examples
#'
#'
#' is_exclusive_freq(c(1,1,0,0,0), c("A", "A", "B", "B", "C"))
#'
#'
#' is_exclusive_freq(c(1,1,0,0,1), c("A", "A", "B", "B", "C"), 2)
#' is_exclusive_freq(c(1,1,0,0,0), c("A", "A", "B", "B", "C"), 1/3)
#'
#'
is_exclusive_freq <- function(.freq, .subgroup=NULL, .exclusive=1, na.rm=FALSE) {

  if (is.null(.subgroup))

    is_in_subgroup <- as.logical(.freq)

  else

    is_in_subgroup <- tapply(.freq, .subgroup,  function(x) any(as.logical(x)))



  if (isFALSE(na.rm))

    if (!any(is_in_subgroup))

      return(NA)



  if (.exclusive >= 1)

    return(sum(is_in_subgroup) <= .exclusive)

  else

    return(mean(is_in_subgroup) <= .exclusive)



}
