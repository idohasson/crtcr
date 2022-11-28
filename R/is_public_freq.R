#' Public clonotype frequencies
#'
#' check if clonotype's frequencies throughout repertoire group are public
#'
#' @param .public Minimum number of repertoires
#' @param ... x
#' @param na.rm x
#'
#' @return logical scalar. TRUE for public, FALSE for private (or none)
#' @export
#'
#' @examples
#'
#' require(dplyr)
#'
#' df <- data.frame(x=factor(LETTERS[sample(1:5, 10,replace = TRUE)]),
#'                  y=factor(letters[sample(1:3, 10, replace = TRUE)]),
#'                  clonotype=sample(c("ASD", "AWD"), 10, replace = TRUE))
#'
#' df %>% <- <- <- <- <- <- <-
#'   group_by(clonotype) %>%
#'   summarise(public=is_public_freq(x,y, .public = .5))
#'
is_public_freq <- function(..., .public=2) {

  clonotype_data <- vctrs::df_list(..., .name_repair = "minimal")

  clonotype_data <- vctrs::new_data_frame(clonotype_data)

  freq <- vctrs::vec_unique_count(clonotype_data)


  if (.public > 1)

    return(freq >= .public)

  else {

    n_levels <- sapply(seq_along(clonotype_data),
                       function(i) length(levels(clonotype_data[,i])))

    return(freq / sum(n_levels) >= .public)

  }

}



# N <- sum(as.logical(freq), na.rm = TRUE)
#
# if (N == 0)
#
#   return(NA)
#
# if (.public <= 1)
#
#   N <- N / length(freq)
#
# N >= .public

# return(freq/length(levels(r_id)))
# return(clonotype_data)
# return(freq / nrow(clonotype_data) >= .public)



# sum(as.logical(freq), na.rm = TRUE) > .public
# N <- sum(freq  >= .min, na.rm = TRUE)






