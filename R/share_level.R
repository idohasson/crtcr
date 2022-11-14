#
# f <- function(..., margin) {
#     if (is.numeric(v))
#       return(sum(as.logical(v), na.rm = TRUE))
#     else
#       return(n_distinct(v, na.rm = TRUE))
#
#   # f1 <- function(v) sum(as.logical(v), na.rm = TRUE)
#   # f2 <- function(v) n_distinct(v, na.rm = TRUE)
#
#
#   if (every(..., is_bare_atomic, n=1)) {
#
#     if (!missing(margin))
#       return(apply(..., margin, f))
#
#     v <- unlist(c(...))
#
#
#   } else if (is.list(...)) {
#     l <- rlang::dots_splice(..., .preserve_empty = TRUE)
#     return(tapply(l[[1]], do.call(cbind, l[-1]), f))
#   }
#
#   # l <- rlang::dots_splice(..., .preserve_empty = TRUE)
#
#   # if (rlang::dots_n(...) == 1) {
#   #
#   #   if (is_bare_vector(...)) {
#   #
#   #     if (is_bare_numeric(...)) {
#   #
#   #       if (is.null(dim(...)))
#   #         return(f1(...))
#   #       else
#   #         return(apply(..., margin, f1))
#   #     }
#   #   }
#   #
#   # } else {
#   #   l <- rlang::dots_splice(..., .preserve_empty = TRUE)
#   #   if (group) {
#   #
#   #     if (n_distinct(lengths(l))==1) {
#   #       return(tapply(l[[1]], do.call(cbind, l[-1]), f))
#   #     } else stop("")
#   #   }
#   # }
#
#   # return(f2(...))
# }
#
# # f.vector <- function(...) {
# #   if (is_bare_vector(...) & is_bare_numeric(...))
# #       return(sum(as.logical(...), na.rm = TRUE))
# #
# #   return(n_distinct(... ,na.rm = TRUE))
# # }
#


#' Share-level of the group's repertoires
#'
#' @param ...
#'
#' @return share-level vector named by clonotype sequence
#' @export
#'
#' @examples
#'
#' rep1 <- c('CGGGTGAAG', 'CGGGTG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG')
#' rep2 <- c('AAGGGGTCCGTC','CGGGTGAAG','AAAGGGTCCGTT','CGGGTCAAG')
#' rep3 <- c('AAGGGGTCAGTC','CGTGTGAAG','AAGGGGTCCGTT','CGGGTCAAG')
#'
#' share_level(rep1, rep2, rep3)
#'
#' share_level(c(rep1, rep2), c(rep2, rep3))
#'
#'
share_level <- function(...) {

  sum(as.logical(c(...)), na.rm = TRUE)

}


#' Compute the share level of clonotypes in a group
#'
#' @param data data frame
#' @param clonotype_var clonotype sequence column name
#' @param rid repertoire identification column name
#' @param ... group by
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' require(dplyr)
#'
#' df <- data.frame(nt=c("ATG","TTC","TAT","TTT","ATG","ATG"),
#'                  aa=c("M","F","Y","F","M","M"),
#'                  rep_id = gl(3,1, 6),
#'                  group_id = gl(2, 3, labels = c("cancer", "control")))
#' df %>%
#' group_by(group_id) %>%
#' share_level_df(aa, rep_id)
#'
#' # Same as:
#' share_level_df(df, aa, rep_id, group_id)
#'
share_level_df <- function(data, clonotype_var, rid, ...) {

  group_by(data, {{clonotype_var}}, ..., .add = FALSE) %>%

  summarise(across({{rid}}, n_distinct, .names = "share"), .groups = "drop")

}

#' Title x
#'
#' @param df x
#' @param clonotype_var x
#' @param rid x
#' @param ... x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' df <- data.frame(nt=c("ATG","TTC","TAT","TTT","ATG","ATG"),
#'                  aa=c("M","F","Y","F","M","M"),
#'                  rep_id = gl(3,1, 6),
#'                  group_id = gl(2, 3, labels = c("cancer", "control")))
#'
#'  share_level_table(df, aa, rep_id, group_id)
#'
share_level_table <- function(df, clonotype_var, rid, ...) {

  # tapply(df[[1]], df[c(2,4)], n_distinct)

  df %>%

  share_level_df({{clonotype_var}}, {{rid}}, ...) %>%

  xtabs(formula = share ~ .)

}
