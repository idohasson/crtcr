NT_AXIS <- c(T=1L, C=1L, A=2L, G=3L)

cr_seq <- function(.nt) {

  nt_vec <- strsplit(.nt, NULL)

  nt_axis <- lapply(nt_vec, get_axis)



}

cr_seq(c("ATGTAA", "TTACCC"))


get_axis <- function(nucleotide) {

  vapply(nucleotide, function(nt) NT_AXIS[nt], integer(1), USE.NAMES = FALSE)

}



get_axis(c("A", "T"))
