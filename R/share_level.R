#' Share level
#'
#' @param ... x
#' @param share_func  x
#' @param na.rm x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   x=c("A", "A", "A", "B", "B", "B"),
#'   y=c("C", "C", "D", "D", "E", "E"),
#'   clonotype=c("a", "b", "a", "b", "a", "b")
#' )
#'
#' share_level(df)
#'
#' require(dplyr)
#'
#' my_func <- function(df) {
#'
#'  table(df) %>%
#'
#'    rowMeans() %>%
#'
#'    sum()
#' }
#'
#' group_by(df, clonotype, x) %>%
#'   summarise(share=share_level(x,y, share_func = my_func))
#'
share_level <- function(.r_id, ..., share_func=NULL, na.rm=FALSE) {

  clonotype_data <- vctrs::df_list(.r_id, ..., .name_repair = "minimal")

  clonotype_data <- vctrs::new_data_frame(clonotype_data)

  if (isTRUE(na.rm)) {

    no_na <- vctrs::vec_detect_complete(clonotype_data)

    clonotype_data <- vctrs::vec_slice(clonotype_data, no_na)

  }

  if (!is.null(share_func))

    return(share_func(clonotype_data))

  vctrs::vec_unique_count(clonotype_data)

}
