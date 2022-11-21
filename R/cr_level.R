#' Convergent recombination level calculation.
#'
#' unique number of values / rows.
#'
#' @param ... x
#' @param cr_func x
#' @param na.rm x
#'
#' @return integer
#' @export
#'
#' @examples
#'
#' cr_level(c("A", "B","A", "B", "B", NA))
#'
#' cr_level(c("s", "s", "a", "a"), c(1:3,NA), na.rm = TRUE)
#'
#' require(dplyr)
#'
#' my_func <- function(df){
#'   df %>%
#'     dplyr::group_by(x) %>%
#'     dplyr::summarise(total=sum(y)) %>%
#'     dplyr::pull() %>%
#'     sd
#'  }
#'
#'  cr_level(x=c("A", "B", "A"), y=1:3, cr_func = my_func)
#'
cr_level <- function(.clonal_seq,..., cr_func=NULL, na.rm=FALSE) {

  clone_data <- vctrs::df_list(.clonal_seq, ..., .name_repair = "minimal")

  clone_data <- vctrs::new_data_frame(clone_data)

  if (isTRUE(na.rm)) {

    no_na <- vctrs::vec_detect_complete(clone_data)

    clone_data <- vctrs::vec_slice(clone_data, no_na)

  }

  if (is.null(cr_func)) {

    results <- vctrs::vec_unique_count(clone_data)

  } else {

    results <- cr_func(clone_data)

  }

  results

}


