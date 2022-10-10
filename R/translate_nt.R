translate <- function(nt_vec) {

  code <- c(T = 0, C = 1, A = 2, G = 3)
  aa_code <- "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG"
  aa_code <- strsplit(aa_code, "")[[1]]


  nt_to_aa <- function(nt_seq) {
    c <- code[nt_seq]
    in_frame <- seq(from=1, to=length(c) ,by=3)
    c <- 16*c[in_frame] + 4*c[in_frame + 1] + c[in_frame + 2] + 1
    paste0(aa_code[c], collapse = "")

  }

  nt_vec %>%
    as.vector %>%
    strsplit(split = "") %>%
    map(nt_to_aa) %>%
    unlist

}
