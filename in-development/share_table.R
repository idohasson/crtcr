
#' Share-level from frequencies
#'
#' @param ...
#'
#' @return share-level vector named by clonotype sequence
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
share_level.freq <- function(...) {

  sum(as.logical(c(...)), na.rm = TRUE)

}

share_level_table <- function(df, clonotype_var, rid, ...) {

  df %>%

    share_level_df({{clonotype_var}}, {{rid}}, ...) %>%

    xtabs(formula = share ~ .)

}
