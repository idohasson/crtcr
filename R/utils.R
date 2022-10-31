#' Translates a DNA sequence into an amino acid sequence.
#'
#' @param nt_vec
#'
#' @return
#'
#' @examples
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

#' Title
#'
#' @param df
#' @param AA_FIELDS
#'
#' @return
#' @export
#'
#' @examples
search_field <- function(df, of) {

  stopifnot(is_named(df))

  of <- arg_match(of, c("aa", "nt"))

  AA_FIELDS <- c("amino_acid", "aaSeqCDR3", "cdr3aa", "CDR3 amino acid sequence",
                 "CDR3.amino.acid.sequence", "CDR3aa", "junction_aa",
                 "CDR3.aa", "AAseq", "Amino acid sequence", "cdrAASeq")

  NT_FIELDS <- c("rearrangement", "nSeqCDR3", "CDR3 nucleotide sequence",
                 "CDR3.nucleotide.sequence", "cdr3nt", "junction", "cdr3_nt",
                 "CDR3.nt", "NNseq", "Junction nucleotide sequence", "cdrNucSeq")

  known_fields <- case_when(of=="aa" ~ AA_FIELDS,
                            of=="nt" ~ NT_FIELDS)

  matched <- has_name(df, known_fields)

  ifelse(any(matched), known_fields[which(matched)][1], NULL)

}

#' Title
#'
#' @param df
#' @param aa_field
#'
#' @return
#' @export
#'
#' @examples
get_aa <- function(df, aa_field) {

  if (missing(aa_field)) aa_field <- search_field(df, "aa")

  if (!is.character(aa_field)) return(NULL)

  pull(df, aa_field)

}


#' Title
#'
#' @param df
#' @param nt_field
#'
#' @return
#' @export
#'
#' @examples
get_nt <- function(df, nt_field) {

  if (missing(nt_field)) nt_field <- search_field(df, "nt")

  if (!is.character(nt_field)) return(NULL)

  pull(df, nt_field)

}



# NT
# immunoseq = "rearrangement"
# MiXCR = "nSeqCDR3"
# mitcr = "cdr3nt"
# VDJtools = "cdr3nt"
# migec = "CDR3 nucleotide sequence"
# migmap = "CDR3 nucleotide sequence"
# tcr = "CDR3.nucleotide.sequence"
# AIRR = "junction"
# 10x_consensus = "cdr3_nt"
# 10x_filt_contigs = "CDR3.nt"
# catt = "NNseq"
# rtcr = "Junction nucleotide sequence"
# imseq = "cdrNucSeq"

# AA
# immunoseq = "amino_acid"
# MiXCR = "aaSeqCDR3"
# mitcr = "cdr3aa"
# migec = "CDR3 amino acid sequence"
# migmap = "cdr3aa"
# tcr = "CDR3.amino.acid.sequence"
# VDJtools = "CDR3aa"
# AIRR = "junction_aa"
# 10x_filt_contigs = "CDR3.aa"
# catt = "AAseq"
# rtcr = "Amino acid sequence"
# imseq = "cdrAASeq"






