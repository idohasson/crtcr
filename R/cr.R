# library(vctrs)
# library(dplyr)
# library(utils)
# library(magrittr)
# library(rlang)
# library(purrr)


#################### Utility functions ####################



# clone_gen <- function() {
#
#   coding_seq <- function(n_codons) {
#
#     coding_codon <- function() {
#       codon <- paste(sample(c("A","G","T","C"), 3, rep = TRUE), collapse = "")
#       ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)
#     }
#     paste(replicate(n_codons, coding_codon()), collapse = "")
#   }
#
#   Vectorize(coding_seq, "n_codons")
# }

#' translate the NT sequence to AA sequence
#'
#' @param nt_vec a character vector
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' translate("ATG")
#'
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

#' Pair NT vector to AA to a new DF
#'
#' @param nt char vector
#'
#' @return df
#' @export
#'
#' @examples
#'
#' nt2df(c("AGT", "ATT"))
#'
nt2df <- function(nt) {
  # check all AGTC
  cbind.data.frame(clone=nt, clonotype=translate(nt))

}


# rand_clone <- clone_gen()
# rand_rep <- function(n, mu=3) rand_clone(rpois(n, mu)+1)
# rand_gruop <- function(rep_n=6, n=10, mu=3) replicate(rep_n, rand_rep(n, mu), simplify = FALSE)
# rand_subgruops <- function(group_n=3, rep_n=6, n=100, mu=3) replicate(group_n, rand_gruop(rep_n, n, mu), simplify = FALSE)

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

# build_df <- function(...) {
#
#   rep <- list2(...)
#   # n_args <- function(...) length(list2(...))
#   # f <- ... %>% list2(...)
#   # if (n_args(l)==1 & n_args(!!!l)>1) l %<>% f(!!!.)
#   if (vec_is_list(rep) & length(rep)==1)
#       rep %<>% pluck(1)
#
#   if (is.vector(rep)) {
#
#     if(is.vector(pluck(rep, 1))) {
#
#       if (is.character(pluck(rep, 1, 1)))
#         rep %<>% modify_depth(2, nt2df)
#
#
#       if (is.data.frame(pluck(rep, 1, 1)))
#         rep %<>% map(bind_rows, .id = "rep_id")
#
#     }
#
#     if(is.data.frame(pluck(rep, 1)))
#         rep %<>% bind_rows(.id = "group")
#   }
#
#   if (length(rep)==3 & is.character(rep[[3]]))
#
#     rep %<>% cbind(clonotype=translate(pull(.)))
#
#   if (!is.data.frame(rep) | ncol(rep) != 4) print("ERROR")
#
#   distinct_at(rep, -4, .keep_all = TRUE) %>%
#
#   set_colnames(c("group", "rep_id", "clone", "clonotype"))
#
# }


#################### share-level table ####################
        # clonotype sharing between individuals #

# Previous studies have shown that the extent of sharing and the
# clonotypic frequency of TCRb sequences are significantly corre
# lated with their production efficiencies in simulations of a random
# recombination process because of the phenomenon of convergent
# recombination. https://doi.org/10.1073/pnas.1319389111

# cr_share <- function(..., by=c("clonotype", "rep_id")) { # DF
#   build_df(...) %>%
#   select_at(by) %>%
#   distinct() %>%
#   table()
#   # as.data.frame.array()
# }

#################### CR-level table ####################

# several clones that encoded the same amino acid sequence were found to be structurally distinct at the nucleotide level, strongly implying clonal selection and expansion is operating at the level of specific TCR-peptide interactions.
# input_reps <- rand_subgruops()
#
# f <- function(n) (max(n, na.rm = TRUE) > 1) + (sum(n != 0, na.rm = TRUE) > 1)
#
# tbl <- cr_share(input_reps, by=c("clonotype", "group"))
# tbl[1:10,] %>%
# apply(1, max, na.rm = TRUE)

# share_tbl <- rand_subgruops() %>% build_df() %>%
#               cr_share(by=c("clonotype", "rep_id"))
# cr_class(input_reps, by=c("clonotype", "group"))



# cr_class <- function(..., public_min=1, exclusive_min=1) { # vector
#   # TODO: check input and generate sharing table if needed
#   share_tbl <- cr_share(...)
#
#   cr_index <- function(n) {
#     #     numeric vector of the unique number of samples
#     #       having a specific clonotype in every group
#     (max(n, na.rm = TRUE) > 1) + (sum(n != 0, na.rm = TRUE) > 1)
#   #   can't be private             multiple shared samples
#   #   public clonotype               inclusive clonotype
#   }
#
#   apply(share_tbl, 1, cr_index) %>%
#   # private = 0 | exclusive = 1 | inclusive = 2
#   {case_when(. == 0 ~ "private",
#             . == 1 ~ "exclusive",
#             . == 2 ~ "inclusive")}
# }

# cr_level <- function(rep_gruops,...) { # DF
#   build_df(rep_gruops,...) %>%
#   with(table(clonotype, group, rep_id)) %>%
#   as.data.frame.array()
# }

# factor_cr <- function(group_count) {
#
#   compute_type <- function(tbl) {
#           # public clonotype                    inclusive clonotype
#           # can't be private                  multiple shared samples
#     (rowSums(tbl, na.rm = TRUE) > 1) + (rowSums(tbl != 0, na.rm = TRUE) > 1)
#           # private = 0   |   exclusive = 1   |   inclusive = 2
#   }
#
#   group_count %>% compute_type() %>%
#   factor(levels = c(0, 1, 2),
#          labels = c("private", "exclusive", "inclusive"))
# }

# cr_list <- function(rep_gruops) {
#   rep_gruops %>% cr_share %>%
#   factor_cr %>% split(x = names(.))
# }

# names_g <- c("Cancer", "Pre-Cancer", "Control")

# pop <- rand_subgruops(3, rpois(1, 4), rpois(1, 1E2), 3)
# rep_df <- build_df(pop)

# mouse <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% build_df() %>% cr_list
# monkey <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% build_df() %>% cr_list
# human <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% build_df() %>% cr_list

# populations <- list(mouse, monkey, human) %>%
# setNames(c("Mouse", "Monkey", "Human"))

# mat_list <- populations %>%
# combn(2, cr_overlap, FALSE)
# library(reshape2)

# lapply(mat_list, melt)
# mat_list %>% lapply(. %>% as.data.frame(row.names = NULL) %>% melt(.id=1:2))
# # %>% melt()
# f <- . %>%
#   # mat_list[[.]] %>%
#   as.data.frame() %$%
#   cbind(rbind(cbind(private, exclusive),
#         cbind(private, inclusive),
#         cbind(exclusive, inclusive)), dimnames(.) %>% names %>% as.character())

# mat_list %>% map(.f = f)

# cr_overlap(populations[1:2]) %>%

# cr_overlap <- function(subpopulations, func=intersect_percentage) { # TODO: overlap as number of intersect. not percentile
#
#   cross(subpopulations) %>%
#
#   lapply(setNames, names(formals(func))) %>%
#
#   invoke_map(.f = func) %>%
#
#   array(dim = c(3,3), dimnames = lapply(subpopulations, names))
#
# }

# aa_vec1 <- rand_rep_vec("aa", 10000, 5)
# aa_vec2 <- rand_rep_vec("aa", 10000, 5)
#
# intersect_percentage(aa_vec1, aa_vec2)
# cr_simmilarity
# intersect_percentage <- function(x, y) { # overlap coefficient
#   length(intersect(x, y)) / min(length(x), length(y))
# }

# TODO: morisita

# cr_dissimilarity
# jaccard_index <- function(x, y){
#   length(intersect(x, y)) / length(union(x, y))
# }



