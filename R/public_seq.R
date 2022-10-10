# library(agvgd)
# package ‘segmented’
# package ‘grantham’
# package ‘seqinr’
# package ‘agvgd’


split_by_col <- function(df, cdr3_nt_col, cdr3_aa_col) {
  rep <- df[c(cdr3_nt_col, cdr3_aa_col)]
  colnames(rep) <- c("nt", "aa")
  aa_to_nt <- split(rep$nt, rep$aa)
  # split 2 vectors in a data frame
  # resulting a named list mapping
  # NT sequences groups to their
  # mutual encoding CDR3 AA sequence
  return(aa_to_nt)
}


split_by_col.list <- function(df_list, cdr3_nt_col, cdr3_aa_col) {

  nt_list <- lapply(df_list, pull, cdr3_nt_col)
  aa_list <- lapply(df_list, pull, cdr3_aa_col)
  mapply(split, nt_list, aa_list,SIMPLIFY = FALSE)

}


source("R/read/read_rep.R")

all_cdr3 <- Reduce(c, rep_list) # Function clusterMap and mcmapply (not Windows) in package parallel provide parallel versions of Map.

rep1 <- split_by_col(group1$BRCA1, "nSeqCDR3", "aaSeqCDR3")
rep2 <- split_by_col(group2$Control1, "nSeqCDR3", "aaSeqCDR3")


