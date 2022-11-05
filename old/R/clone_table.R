

clone_table <- function(df, clone_nt, clonotype, to_vec=FALSE) {

  stopifnot(is.data.frame(df))

  if (missing(clone_nt)) clone_nt <- search_field(df, "nt")

  if (!hasName(df, clone_nt)) stop("couldn't find columns")


  clone_nt_df <- distinct_at(df, clone_nt, .keep_all = TRUE)


  # if (ncol(clone_nt_df)==2) return(distinct(clone_nt_df))

  if (missing(clonotype)) clonotype <- search_field(clone_nt_df, "aa")

  if (!hasName(clone_nt_df, clonotype)) {
    clonotype
  }

  # if(to_vec) return(clonotype_aa)



  # if (!is.character(clonotype)) return(cbind.data.frame(clone_nt=clone_nt_df, translate(clone_nt)))



}


clone_nt_aa <- function(nt, aa) {


  # cbind.data.frame(clone_nt=clone_nt_df, translate(clone_nt))

  if(!missing(aa))
    aa[!duplicated(nt)]
  else
    translate(vec_unique(nt))
}




translate <- function(nt_vec) {
  # converts the nucleotide vector to uppercase
  nt_vec <- toupper(nt_vec)
  # checks that each element in the vector is a DNA base.
  stopifnot(all(grepl(pattern = "^[AGTC]+$", nt_vec)))

  # a vector of integers that correspond to the nucleotide characters.
  encoding <- c(T = 0, C = 1, A = 2, G = 3)
  # a vector of characters that correspond to the amino acid characters.
  decoding <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]
  # function for converting a DNA character vector into an amino acid character vector.
  nt_to_aa <- function(nt) decoding[encoding[nt[seq(1, length(nt) ,3)]] * 16 +
                                      encoding[nt[seq(2, length(nt) ,3)]] * 4 +
                                      encoding[nt[seq(3, length(nt) ,3)]] + 1]

  # splits the nucleotide vector into a character vector of individual nucleotides
  strsplit(nt_vec, split = "") %>%
    # converts each nucleotide character to an amino acid character.
    lapply(nt_to_aa) %>%
    # pastes the amino acid characters together into a single string.
    sapply(paste, collapse="")
}
