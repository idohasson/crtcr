
#' Title x
#'
#' @param ... x
#' @param .exclusive x
#' @param na.rm x
#'
#' @return x
#' @export
#'
#' @examples
#'
#'
#' require(dplyr)
#'
#' df <- data.frame(x=factor(LETTERS[sample(1:5, 5,replace = TRUE)]),
#'                  y=factor(letters[sample(1:2, 5, replace = TRUE)]),
#'                  clonotype=sample(c("ASD", "AWD"), 5, replace = TRUE, prob = c(.2,.8)))
#'
#' df %>%
#'   group_by(clonotype) %>%
#'   summarise(
#'     public=is_public(x,y),
#'     exclusive=is_exclusive(y)
#'   )
#'
#'
#'
is_exclusive <- function(..., .exclusive=1) {

  clonotype_data <- vctrs::df_list(..., .name_repair = "minimal")

  clonotype_data <- vctrs::new_data_frame(clonotype_data)

  freq <- vctrs::vec_unique_count(clonotype_data)

  if (.exclusive >= 1)

    return(freq <= .exclusive)

  else {

    n_levels <- sapply(seq_along(clonotype_data),
                       function(i) length(levels(clonotype_data[,i])))

    return(freq / sum(n_levels) <= .exclusive)

  }

}
