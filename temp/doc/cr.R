# library(vctrs)
# library(dplyr)
# library(utils)
# library(magrittr)
# library(rlang)
# library(purrr)

#################### Clone Table ####################

# # df_lists <- replicate(4, rand_group(), FALSE)
# # group_df <- group_join(df_lists)
# # head(group_df)
# # clone_table(group_df, clonotype = "clonotype", by_col="group")
# clone_table <- function(df, clonotype, by_col, ...) {
#
#   unique_by <- dplyr::vars(c(clonotype, by_col, ...))
#
#   dplyr::distinct_at(df, all_of(unique_by), .keep_all = TRUE) %>%
#
#     dplyr::select(clonotype, by_col) %>% table
#
# }


#' #################### share-level table ####################
#' # clonotype sharing between individuals #
#'
#' # Previous studies have shown that the extent of sharing and the
#' # clonotypic frequency of TCRb sequences are significantly corre
#' # lated with their production efficiencies in simulations of a random
#' # recombination process because of the phenomenon of convergent
#' # recombination. https://doi.org/10.1073/pnas.1319389111
#'
#'
#'
#' #' Title
#' #'
#' #' @param ... data frames or vectors
#' #'
#' #' @return table
#' #'
#' #' @importFrom dplyr distinct
#' #' @importFrom magrittr %>% %$%
#' #'
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' # generate random
#' #' df_lists <- replicate(4, rand_group(), FALSE)
#' #' rand_df <- group_join(df_lists)
#' #' names(rand_df)
#' #'
#' sharing <- function(...) { # DF
#'   # a vector of integers giving the numbers of the variables, or a character vector giving the names of the variables to be used for the columns of the flat contingency table.
#'
#'   # Clonotype
#'   df <- group_join(...)
#'
#'
#'
#'   # unique_clontype_df <- distinct_at(df, all_of())
#'
#'   # sharing()
#'   clonotype_share_level <- unique_clontype_df %>%
#'
#'       select_at(all_of(vars(c("clonotype", "group")))) %>%
#'
#'       table
#'
#'   clonotype_share_level
#'
#'   # Clone
#'   unique_clone_df <- distinct_at(df, all_of(vars(c("group", "rep_id", "clone"))), .keep_all = TRUE)
#'
#'   clonal_seq_share_level <- unique_clone_df %>%
#'
#'     select_at(all_of(vars(c("clone", "group")))) %>%
#'
#'     table
#'
#'   clonal_seq_share_level
#'
#'   # distinct(clonotype, group) %$%
#'   # table(clonotype, group)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'


#' #################### share-level table ####################
#
#
# set.seed(10)
# dat <- data.frame(grp1 = sample(c(0, 1), size = 5, replace = TRUE),
#                   grp2 = sample(c(0, 1), size = 5, replace = TRUE),
#                   grp3 = sample(c(0, 1), size = 5, replace = TRUE),
#                   value = round(runif(5, min = 0, max = 10), 0) )
# dat
#
# library(dplyr)
# library(tidyr)
#
# dat %>%
#   pivot_longer(grp1:grp3, names_repair = 'unique') %>%
#   filter(value...3 == 1) %>%
#   group_by(name) %>%
#   summarise(means = mean(value...1))
#
#
# # spec2 <- tibble(
# #   .name = c("income", "rent", "income_moe", "rent_moe"),
# #   .value = c("estimate", "estimate", "moe", "moe"),
# #   variable = c("income", "rent", "income", "rent")
# # )
# #
# # us_rent_income %>%
# #   pivot_wider_spec(spec2)
#
# head(rand_df)
#
# # averaged CR-level per group
# share_f1 <- function(x,y) {
#   dplyr::n_distinct(x, y) / dplyr::n_distinct(x)
# }
#
# # tatal CR-level - sum of all CR level of the the group
# share_f2 <- function(x,y) {
#   n_distinct(x, y)
# }
#
# # tatal CR-level - unique clone number
# share_f3 <- function(x,y) {
#   n_distinct(y)
# }
#
# # shared by all in the group
# share_f4 <- function(x,y) {
#   length(Reduce(intersect, split(x,y)))
# }
#
#
# # TODO:
# # relative frequency
# # share_f5 <- function(x,y) {
# #   length(Reduce(intersect, split(x,y)))
# # }
#
#
# # test_data = tibble(a = sample(c("group 1","group 2"), 100, replace=T),
# #                    b = rnorm(100))
#
# test_fun = function(data, group, value1, value2) {
#
#   data %>%
#
#     group_by({{ group }}) %>%
#
#     summarize(mean=n_distinct({{ value1 }}))
# }
# # test_data = as_tibble(rand_df)
#
# test_fun(test_data, group, rep_id, clone)
#
# # test_fun(test_data, a, b)
#
#
#
# share_level <- function(df, gid, rid, clone, clonotype) {
#   # library(dplyr)
#   # library(magrittr)
#   # share_f1 <- function(x,y) dplyr::n_distinct(x, y) / dplyr::n_distinct(x)
#
#   data %>% group_by({{ gid}} ) %>% summarize(mean=n_distinct({{ clone }}))
#
#
#   nms <- setdiff(nms, group_vars(unique_by))
#   data %>% dplyr::summarise_at(dplyr::group_vars(rid, clone), h)
#   ?dplyr::vars()
#   # return(unique_by)
#
#   dplyr::group_by_at(df, .vars = to_each) %>%
#
#   dplyr::select_at(unique_by) %>%
#
#   dplyr::summarise_at(.vars = unique_by, f(rid, clone)) %>%
#
#   as.data.frame()
#
# }
#
# share_level(rand_df, "group", "rep_id", "clone", "clonotype") %>% head
#
# rand_df %>%
#   group_by(clonotype, group) %>%
#   summarise(Share_level = f(rep_id, clone), .groups = "drop") %>%
#   as.data.frame()
#
#
# groups_tbl <- rand_df %>% group_by(clonotype, group) %>% select(rep_id, clone) %>%
#   summarise(Share_level = n_distinct(rep_id),
#             total_CR_levle = n_distinct(rep_id, clone),
#             share_levle2 = share_f1(rep_id, clone),
#             averaged_CR_levle = n_distinct(rep_id, clone) / Share_level) %>%
#   filter(clonotype %in% c("A", "K", "D", "F"))
#
# pivot_wider(groups_tbl, id_cols = "clonotype", names_from ="group" , values_from = "Share_level")
#
#
#
#
#
#
# rand_df %>% distinct(group, rep_id) %>% group_by(group) %>% summarise(n=n_distinct(rep_id))



#   CR-level requires - at least one repertoire
#   Share-level requires - at least one group
#   CR-class requires - at least two groups
#   CR-ts (time series) requires - at least two groups
#   TODO CR-ts (?)
#
#
# valid CR-class - |GROUPS| > 1, |REPERTOIRES| > 0; NT|(NT+AA) per repertoire
#
#   nt (character vector) - clonal sequence is one of the unique collection of
#                           CDR3s coding sequences of a specific CDR3-peptide
#   aa (character vector) - clonotype is the CDR3-peptide sequence originated by
#                           either one of its clones
#
#   data type specification (either one of these choices)
#
#   character / factors
#   - a depth-two-list - list of list of NT-vectors. a group is considered as list of NT-vectors)
#
#   data.frame
#   - column order should be
#     1. gid (always first)
#     2. rid (only second to gid)
#     3. nt (always before aa or last for missing aa)
#     4. aa (always last) - is optional, and if not provided, calculated as last column out of NT column codon translation
#   - provide column name by gid, rid, clone_col, clonotype_col for any additional column
#
#   1. list of list of data.frames
#     - character vector
#     - factor
#     - two columns named 'nt' and 'aa'
#     - 1 column NT
#   2. list of data.frames
#     - a column named 'gid' + a column named 'nt'
#     - a column named 'gid' + two column named 'nt' and 'aa'
#   3.data.frame
#     - two columns named 'gid' and 'rid' + a column 'nt'
#     - two columns named 'gid' and 'rid' + two columns named 'nt' and 'aa'

# AA_FIELDS <- c("amino_acid", "aaSeqCDR3", "cdr3aa", "CDR3 amino acid sequence",
#                "CDR3.amino.acid.sequence", "CDR3aa", "junction_aa",
#                "CDR3.aa", "AAseq", "Amino acid sequence", "cdrAASeq")
#
# NT_FIELDS <- c("rearrangement", "nSeqCDR3", "CDR3 nucleotide sequence",
#                "CDR3.nucleotide.sequence", "cdr3nt", "junction", "cdr3_nt",
#                "CDR3.nt", "NNseq", "Junction nucleotide sequence", "cdrNucSeq")



#' CR level table
#'
#' @param ... repertoire list / data frames vectors
#'
#' @return table
#'
#' @importFrom dplyr distinct_at n_distinct
#' @importFrom tidyr unite pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'
#' # generate random
#' df_lists <- replicate(4, rand_group(), FALSE)
#' rand_df <- group_join(df_lists)
#' names(rand_df)
#' unite(col = "id", c("rep_id" ,"group"), sep = "/")


# cr_level(df = rand_df, clone = "clone", clonotype = "clonotype", rep_id = "rep_id" ,"group")


#' id_df <- unite(rand_df, col = "id", c("group", "rep_id"), sep = "/")
#' cr_level(df = id_df, clone = "clone", clonotype = "clonotype", rep_id = "id")
#'
# cr_level <- function(df, clone, clonotype, rep_id, ...) { # TODO: allow data frame input of 'group_join' output
#
#
#   clone_table(id_df, clonotype = clonotype, by_col = id, clone, ...)
#
#   # unique_clone_df <- distinct_at(df, all_of(vars(c("group", "rep_id", "clone"))), .keep_all = TRUE)
#   #
#   # cr_level <- unite(unique_clone_df, col = "id", c("group", "rep_id"), sep = "/") %>%
#   #
#   #   select_at(all_of(vars(c("clonotype", "id")))) %>% table
#   #
#   # cr_level
#
#   unite(df, group, rep_id, col = "id") %>%
#   distinct_at(c("id", "clone"), .keep_all=TRUE) %>%
#   pivot_wider(names_from = id,
#                      values_from = clone,
#                      values_fn = n_distinct) %>%
#   column_to_rownames("clonotype") %>% as.matrix()
#
# }



# clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")
# df_lists <- replicate(4, rand_group(), FALSE)
# group_df <- group_join(df_lists)
#
# clonotype_share_level <- unique_clontype_df %>%
#
#   select_at(all_of(vars(c("clonotype", "group")))) %>%
#
#   table
# # head(group_df)
# clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")
# clone_table(group_df, clonotype = "clonotype", by_col="group", "rep_id")






#################### Utility functions ####################
#'
#' #'
#' #'
#' #' #' generate sequence with coding codons
#' #' #'
#' #' #' @return string
#' #' #' @export
#' #' #'
#' #' #' @examples
#'
#' #' #' nt_gen <- clone_gen()
#' #' #' nt_gen(3)
#' #' clone_gen <- function() {
#' #'
#' #'   coding_seq <- function(n_codons) {
#' #'
#' #'     coding_codon <- function() {
#' #'       codon <- paste(sample(c("A","G","T","C"), 3, replace = TRUE), collapse = "")
#' #'       ifelse(codon %in% c("TGA","TAA","TAG"), coding_codon(), codon)
#' #'     }
#' #'     paste(replicate(n_codons, coding_codon()), collapse = "")
#' #'   }
#' #'
#' #'   Vectorize(coding_seq, "n_codons")
#' #' }
#' #'
#'


#' #' translate the NT sequence to AA sequence
#' #'
#' #' @param nt_vec a character vector
#' #'
#' #' @return a character vector
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' translate("ATG")
#' #'
#' translate <- function(nt_vec) {
#'   # converts the nucleotide vector to uppercase
#'   # nt_vec <- toupper(nt_vec)
#'   # checks that each element in the vector is a DNA base.
#'   stopifnot(all(grepl(pattern = "^[AGTC]+$", nt_vec)))
#'
#'   # a vector of integers that correspond to the nucleotide characters.
#'   encoding <- c(T = 0, C = 1, A = 2, G = 3)
#'   # a vector of characters that correspond to the amino acid characters.
#'   decoding <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]
#'   # function for converting a DNA character vector into an amino acid character vector.
#'   nt_to_aa <- function(nt) decoding[encoding[nt[seq(1, length(nt) ,3)]] * 16 +
#'                                       encoding[nt[seq(2, length(nt) ,3)]] * 4 +
#'                                       encoding[nt[seq(3, length(nt) ,3)]] + 1]
#'
#'   # splits the nucleotide vector into a character vector of individual nucleotides
#'   nt_vec <- strsplit(nt_vec, split = "")
#'     # converts each nucleotide character to an amino acid character.
#'   aa_vec <- lapply(nt_vec, nt_to_aa)
#'     # pastes the amino acid characters together into a single string.
#'   sapply(aa_vec, paste, collapse="")
#' }



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

# group_join <- function(...) {
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


# #################### share-level table ####################
#         # clonotype sharing between individuals #
#
# # Previous studies have shown that the extent of sharing and the
# # clonotypic frequency of TCRb sequences are significantly corre
# # lated with their production efficiencies in simulations of a random
# # recombination process because of the phenomenon of convergent
# # recombination. https://doi.org/10.1073/pnas.1319389111
#
# cr_share <- function(..., by=c("clonotype", "rep_id")) { # DF
#   group_join(...) %>%
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

# share_tbl <- rand_subgruops() %>% group_join() %>%
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
#   group_join(rep_gruops,...) %>%
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
# rep_df <- group_join(pop)

# mouse <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% group_join() %>% cr_list
# monkey <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% group_join() %>% cr_list
# human <- list(Cancer=rand_gruop(1000), Control=rand_gruop(1000)) %>% group_join() %>% cr_list

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




