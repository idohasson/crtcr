# library(vctrs)
# library(dplyr)
# library(utils)
# library(magrittr)
# library(rlang)

new_clone <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(x, class = "clone")
}

clone <- function(x = character()) {
  x <- vec_cast(x, character())
  new_clone(x)
}

is_clone <- function(x) {
  inherits(x, "clone")
}

as_clone <- function(x, ...) {
  UseMethod("as_clone")
}

as_clone.default <- function(x, ...) {
  vec_cast(x, new_clone())
}

as_clone.character <- function(x) {
  value <- as.character(toupper(x))
  new_clone(value)
}

#################### Utility functions ####################


clone_gen <- function() {

  coding_seq <- function(n_codons) {

    coding_codon <- function() {
      codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")
      ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)
    }
    paste(replicate(n_codons, coding_codon()), collapse = "")
  }
  Vectorize(coding_seq, "n_codons")
}

translate <- function(nt_vec) {
  # converts the nucleotide vector to uppercase
  # nt_vec <- toupper(nt_vec)
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
  nt_vec <- strsplit(nt_vec, split = "")
    # converts each nucleotide character to an amino acid character.
  aa_vec <- lapply(nt_vec, nt_to_aa)
    # pastes the amino acid characters together into a single string.
  sapply(aa_vec, paste, collapse="")
}

nt2df <- function(nt) {
  # check all AGTC
  cbind.data.frame(clone=nt, clonotype=translate(nt))

}


rand_clone <- clone_gen()
rand_rep <- function(n, mu=3) rand_clone(rpois(n, mu)+1)
rand_gruop <- function(rep_n=6, n=10, mu=3) replicate(rep_n, rand_rep(n, mu), simplify = FALSE)
rand_subgruops <- function(group_n=3, rep_n=6, n=100, mu=3) replicate(group_n, rand_gruop(rep_n, n, mu), simplify = FALSE)
rand_subgruops() %>% cr_share_table()
# rand_gruop() %>% map(nt2df)

#################### Manual ####################

#   Clonal sequence: a string includes only of A, G, C, T characters
#
#   Clonotype: The CDR3's sequence as a string composed of the amino acid
#   character symbols (not strict for additional symbols).
#
#   Repertoire: - a collection of CDR3s' clones. Clone sequences are considered
#   identical by their NT sequence. Distinction attributes such as V/J segments
#   might be added in later versions. any pair of sequences or length-one data
#   types pair should work as well if provided as data frame with two columns
#
#
#
#   'rep_vec' - clonotype vector construction
#
#   Function arguments:
#   - Character vector - clonal sequences which are of strings composed of
#     A, G, C and T characters only as the codons (DNA) for translation.
#   - Data frame, first column with the clonal sequence (NT strings as described
#     in previous option) and the corresponding clonotype sequence (AA) in
#     second column
#
#
#
#
#   'rep_set' (table of 'rep_vec') - clonotype table construction
#
#   Group - a collection of repertoires:
#
#   Function arguments:
#   - list of character vectors
#   - list of data frames
#   - data frame
#
#
#
#
#   'cr_vec' ('rep_set' CR-type vector)
#
#   ## Required for convergent recombination public classification (CR-class)
#   3D entry representing clone:
#   - Clonal sequence (NT) + optional addition of the corresponding clonotype
#   sequence (AA) to avoid translation computation
#   - Repertoire identification.
#   - Group identification.

#   Sub-Groups - sub-sets of the collection repertoires:
#
#   Function arguments:
#   - Depth-two-list of ('cr_vec'):
#     - a group of repertoires as list of ('rep_set' -> 'cr_vec') :
#       - character vectors ('rep_vec' -> 'rep_set' -> 'cr_vec') - NT vectors
#       - data frames ('rep_vec' -> 'rep_set' -> 'cr_vec') - two-columns-Data-frames REP_ID + NT (order-sensitive)
#     - data frame ('rep_set' -> 'cr_vec') - List of Three-columns-Data-frames REP_ID + NT + AA  (order-sensitive)
#
#   - list of one of the forms:
#     - character vectors + list of indices ('rep_set' -> 'cr_vec') - to the sub-group each vector (clonal sequences) belongs
#     - data frames + list of indices ('rep_set' -> 'cr_vec') - to the sub-group each data frame (clonal sequences + clonotype) belongs
#
#   ('cr_vec')
#   - data frame - Three-columns-Data-frame (order-sensitive) GROUP_ID + REP_ID + NT (or GROUP_ID + REP_ID + NT + AA)
#


#################### Data-type ####################




#################### Prepare input data ####################

build_df <- function(rep,...) {

  rep <- list2(rep,...)

  if (vec_is_list(rep) & length(rep)==1)
      rep %<>% pluck(1)

  if (is.vector(rep)) {

    if(is.vector(pluck(rep, 1))) {

      if (is.character(pluck(rep, 1, 1)))
        rep %<>% modify_depth(2, nt2df)


      if (is.data.frame(pluck(rep, 1, 1)))
        rep %<>% map(bind_rows, .id = "rep_id")

    }

    if(is.data.frame(pluck(rep, 1)))
        rep %<>% bind_rows(.id = "group")
  }

  if (length(rep)==3 & is.character(rep[[3]]))

    rep %<>% cbind(clonotype=translate(pull(.)))

  if (!is.data.frame(rep) | ncol(rep) != 4) print("ERROR")

  distinct_at(rep, -4, .keep_all = TRUE) %>%

  set_colnames(c("group", "rep_id", "clone", "clonotype"))

}


#################### share-level table ####################
cr_share_table <- function(rep_gruops,...) { # DF
  build_df(rep_gruops,...) %>%
  with(table(clonotype, group)) %>%
  as.data.frame.array()
}

#################### CR-level table ####################
cr_level <- function(rep_gruops,...) { # DF
  build_df(rep_gruops,...) %>%
  with(table(clonotype, group, rep_id)) %>%
  as.data.frame.array()
}











