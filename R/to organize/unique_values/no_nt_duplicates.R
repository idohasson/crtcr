# sapply(x, no_nt_duplicates)
# sapply(unique_x, no_nt_duplicates)
no_nt_duplicates <- function(nt) {

  anyDuplicated(nt)==0

}
