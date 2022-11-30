#' Amount of unique values overall.
#'
#' @param ... Same length vectors of values.
#' @param na.rm if TRUE, ignore aligned values if any of them are NA. For FALSE,
#' the default, NA is treated as an unique value.
#'
#' @return An integer of unique value count.
#' @export
#'
#' @examples
#'
#' v1 <- c("A", "A", "A", "B")
#' v2 <- factor(c("seqA", "seqA", "seqB", "seqB"))
#' v3 <- c(1,NA,2,2)
#'
#' unique_n(v1)
#' unique_n(v1, v2)
#' unique_n(v1, v2, v3) # NA in v3 is considered as a unique value.
#' unique_n(v1, v2, v3, na.rm = TRUE)
#'
unique_n <- function(..., na.rm = FALSE) {

  dfl <- df_list(..., .name_repair = "minimal")

  dfi <- new_data_frame(dfl)

  if (isTRUE(na.rm)) {

    no_na <- vec_detect_complete(dfi)

    dfi <- vec_slice(dfi, no_na)

  }

  vec_unique_count(dfi)

}








