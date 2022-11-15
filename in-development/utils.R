#################### Utility ####################

AA_FIELDS <- c("amino_acid", "aaSeqCDR3", "cdr3aa", "CDR3 amino acid sequence",
               "CDR3.amino.acid.sequence", "CDR3aa", "junction_aa",
               "CDR3.aa", "AAseq", "Amino acid sequence", "cdrAASeq")

NT_FIELDS <- c("rearrangement", "nSeqCDR3", "CDR3 nucleotide sequence",
               "CDR3.nucleotide.sequence", "cdr3nt", "junction", "cdr3_nt",
               "CDR3.nt", "NNseq", "Junction nucleotide sequence", "cdrNucSeq")

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

get_aa <- function(df, aa_field) {

  if (missing(aa_field)) aa_field <- search_field(df, "aa")

  if (!is.character(aa_field)) return(NULL)

  pull(df, aa_field)

}

get_nt <- function(df, nt_field) {

  if (missing(nt_field)) nt_field <- search_field(df, "nt")

  if (!is.character(nt_field)) return(NULL)

  pull(df, nt_field)

}

rep_prepare <- function(...) {

  if(rlang::dots_n(...) > 1)
    data <- rlang::dots_values(...)
  else
    data <- rlang::dots_splice(...)

  depth <- purrr::vec_depth(data)
  return(data)
  if (depth==1) {
    data <- rep2DF(data)
  } else if (depth==2) {
    data <- repList2DF(data)
  } else if (depth==3) {
    data <- groupList2DF(data)
  } else stop("invalid list depth")

  data
}


#' repertoire's clonal sequence to data frame with clonotype sequence
#'
#' @param clonal_seq character vector / data frame
#' @param clone_var (optional) for data frame with more than one column, provide the CDR3's clonal sequence (nucleation sequence) column name
#'
#' @return data frame
#'
#' @examples
#'
#' clonal_seq <- c("ATG", "TTC", "TTC", "TTT", "ATG", "ATG")
#'
#' clonalSeq2DF(clonal_seq)
#'
#'
clonalSeq2DF <- function(clonal_seq, clone_var) {

  if (!is.character(clonal_seq)) {

    if (missingArg(clone_var))
      clonal_seq <- pull(clonal_seq)
    else
      clonal_seq <- pull(clonal_seq, clone_var)
  }
  data.frame(clone=clonal_seq, clonotype=translate(clonal_seq))
}

