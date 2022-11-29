
#' Is public clonotype by frequencies
#'
#' @description
#'
#' Returns True if clonotype's number of frequency values which are non-zero throughout repertoires / group, meet with the condition. if so, the clonotype is public
#'
#' @param .freq numerical vector
#' @param .public value count threshold
#' @param na.rm if FALSE (the default), return NA when no frequency value meet with the condition
#'
#' @return TRUE for public, False for private clonotype
#' @export
#'
#' @examples
#'
#' is_public_freq(c(0,0,1)) # FALSE
#' is_public_freq(c(1,2,0)) # TRUE
#' is_public_freq(c(0,0,0)) # NA
#' is_public_freq(c(0,0,0), na.rm = TRUE) # FALSE
#'
#' is_public_freq(c(0,0,1), .5) # FALSE
#' is_public_freq(c(1,2,0), .5) # TRUE
#' is_public_freq(c(1,2,0), .75) # FALSE
#'
is_public_freq <- function(.freq, .public=2, na.rm=FALSE) {

  is_in_rep <- as.logical(.freq)

  if (isFALSE(na.rm)) {

    if (!any(is_in_rep))

      return(NA)

  }

  if (.public > 1) {

    return(sum(is_in_rep) >= .public)

  } else {

    return(mean(is_in_rep) >= .public)

  }

}





# is_freq <- function(.freq, .freq_func, .condition, na.rm=FALSE) {
#
#   is_non_zero <- as.logical(.freq)
#
#   if (isFALSE(na.rm)) {
#
#     if (!any(is_non_zero))
#
#       return(NA)
#
#   }
#
#   val <- .freq_func(is_non_zero)
#
#   .condition(val)
#
# }
#
# foo <- as.function(alist(a = , b = 2, a >= b))
#
# # foo(3,7)

# N <- sum(is_in_rep, na.rm = na.rm)
#
# if (N == 0) {
#
#   return(NA)
#
# } else if (.public <= 1) {
#
#   N <- N / length(freq)
#
# } else {
#
#   N >= .public
#
# }

# return(freq/length(levels(r_id)))
# return(clonotype_data)
# return(freq / nrow(clonotype_data) >= .public)



# sum(as.logical(freq), na.rm = TRUE) > .public
# N <- sum(freq  >= .min, na.rm = TRUE)






