
library("data.table")

clone_table <- function(df) {
  df %>% distinct(nSeqCDR3, aaSeqCDR3, sample, group) %>%
    with(table(aaSeqCDR3, sample, group)) %>%
    addmargins

}

cr_level <- function(df) {
  ct <- clone_table(df)
  x[,-ncol(x),"Sum"]
}

sharing_level <- function(df) {

  ct <- clone_table(df)

  ct[,"Sum",-3]
}

is.public <- function(df) {

  ct <- clone_table(df)

  p <- ct[,-ncol(ct),"Sum"] %>%
    apply(2, as.logical) %>%
    rowSums()!=1
  ct[p,-ncol(ct),"Sum"]
}
