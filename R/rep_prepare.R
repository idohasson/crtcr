# https://search.r-project.org/CRAN/refmans/datawizard/html/00Index.html
# # library("datawizard")
#
#
# #' Join all repertoires' clonotypes data into one data frame
# #'
# #' @param ... repertoire group
# #'
# #' @return data frame
# #'
# #' @importFrom rlang list2
# #' @importFrom dplyr bind_rows mutate
# #' @importFrom purrr modify_depth map pluck
# #' @importFrom magrittr %<>% %>%
# #'
# #' @export
# #'
# #' @examples
# #'
# #' # generate random
# #' g <- replicate(4, rand_group(), simplify = FALSE)
# #'
# #' group_join(g)
# #'
# group_join <- function(...) {
#
#   rep <- list2(...) # TODO: dots_list
#   # TODO check valid input
#   # TODO make sure rep values are unique valid input
#   if (is.vector(rep)) {
#
#     if (is.vector(pluck(rep, 1))) {
#
#       if (is.list(rep) & length(rep)==1)
#         rep %<>% pluck(1)
#
#       if (is.character(pluck(rep, 1, 1)))
#         rep %<>% modify_depth(2, function(nt)
#           cbind.data.frame(clone=nt,
#                            clonotype=translate(nt)))
#
#       if (is.data.frame(pluck(rep, 1, 1)))
#         rep %<>% map(bind_rows, .id = "rep_id")
#     }
#
#     if(is.data.frame(pluck(rep, 1)))
#       rep %<>% bind_rows(.id = "group")
#
#   } else if (is.data.frame(rep)) {
#
#     if (is.character(rep[1,3]) & ncol(rep)==3)
#       rep %<>% mutate(clonotype=translate(pluck(3)))
#
#   } else stop("Invalid input error")
#
#   # # TODO check valid input
#   # if (ncol(rep) == 4)
#   #   colnames(rep) <- c("group", "rep_id", "clone", "clonotype")
#   # else stop("ERROR")
#
#  rep
#
# }
#
#
#
# #' translate the NT sequence to AA sequence
# #'
# #' @param nt_vec a character vector
# #'
# #' @return a character vector
# #' @export
# #'
# #' @examples
# #'
# #' translate("ATG")
# #'
# translate <- function(nt_vec) {
#   # converts the nucleotide vector to uppercase
#   # nt_vec <- toupper(nt_vec)
#   # checks that each element in the vector is a DNA base.
#   stopifnot(all(grepl(pattern = "^[AGTC]+$", nt_vec)))
#
#   # a vector of integers that correspond to the nucleotide characters.
#   encoding <- c(T = 0, C = 1, A = 2, G = 3)
#   # a vector of characters that correspond to the amino acid characters.
#   decoding <- strsplit("FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG", "")[[1]]
#   # function for converting a DNA character vector into an amino acid character vector.
#   nt_to_aa <- function(nt) decoding[encoding[nt[seq(1, length(nt) ,3)]] * 16 +
#                                       encoding[nt[seq(2, length(nt) ,3)]] * 4 +
#                                       encoding[nt[seq(3, length(nt) ,3)]] + 1]
#
#   # splits the nucleotide vector into a character vector of individual nucleotides
#   nt_vec <- strsplit(nt_vec, split = "")
#   # converts each nucleotide character to an amino acid character.
#   aa_vec <- lapply(nt_vec, nt_to_aa)
#   # pastes the amino acid characters together into a single string.
#   sapply(aa_vec, paste, collapse="")
# }
#
# #' #' convert list of nucleation sequences to data frame
# #' #'
# #' #' @param rep_list list
# #' #' @param rep_names sample names
# #' #'
# #' #' @return data frame
# #' #' @export
# #' #'
# #' #' @examples
# #' #'
# #' #' nt_list <- rand_group()
# #' #'
# #' #' group_df <- replist2DF(nt_list, rep_names=LETTERS[seq_along(nt_list)])
# #' #'
# #' #' head(group_df)
# #' #'
# #' replist2DF <- function(rep_list, rep_names) {
# #'
# #'   if (missingArg(rep_names))
# #'
# #'     rep_list %<>% set_names(rep_names)
# #'
# #'   map_dfr(rep_list, . %>%
# #'
# #'   bind_cols(clone=., clonotype=translate(.)) %>%
# #'
# #'   distinct, .id = "rid")
# #'
# #' }
#
#
#
#
#
#
#
#
#

# nt_vec <- c("ATG", "TTC", "TTT", "TTT", "ATG", "ATG")
#
# (vec_list <- replicate(n = 2, nt_vec, simplify = FALSE))
# rep_prepare(vec_list)
#
# (d2_vec_list <- replicate(n = 2, vec_list, simplify = FALSE))
# rep_prepare(d2_vec_list)
#
#
# df <- data.frame(nt=c("ATG","TTC","TTT","TTT","ATG","ATG"), aa=c("M","F","F","F","M","M"))
#
# (df_list <- replicate(n = 2, df, simplify = FALSE))
# rep_prepare(df_list, nt, aa)
# rep_prepare(df_list, nt)
#
# (d2_df_list <- replicate(n = 2, vec_list, simplify = FALSE))
# rep_prepare(d2_df_list, nt, aa)
# rep_prepare(d2_df_list, nt)
#
#
# replicate(n = 2, simplify = FALSE) %>%
# rep_prepare()


#' Title x
#'
#' @param ... x
#'
#' @return x
#' @export
#'
#' @examples
#'
#' 1
#'
#'
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


# c("ATG", "TTC", "TTT", "TTT", "ATG", "ATG") %>%
#   data.frame(x=., y=translate(.)) %>% rep_prepare(clone_var="x", clonotype_var="y")

#' repertoire's clonal sequence to data frame with clonotype sequence
#'
#' @param clonal_seq character vector / data frame
#' @param clone_var (optional) for data frame with more than one column, provide the CDR3's clonal sequence (nucleation sequence) column name
#'
#' @return data frame
#' @export
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



#' repertoire to data frame with clonal sequence and clonotype sequence
#'
#' @param rep character vector or dataframe
#' @param clone_var column name of the clonal sequences
#' @param clonotype_var (optional) column name of the clonotype sequences
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' clonal_seq <- c("ATG", "TTC", "TTC", "TTT", "ATG", "ATG")
#' rep2DF(clonal_seq)
#'
#' df1 <- data.frame(cdr3_nt=clonal_seq)
#' rep2DF(df1)
#' rep2DF(df1, clone_var=cdr3_nt)
#'
#' clonotype_seq <- c("M", "F", "F", "F", "M", "M")
#'
#' df2 <- data.frame(cdr3_nt=clonal_seq, cdr3_aa=clonotype_seq)
#' rep2DF(df2, clone_var=cdr3_nt, clonotype_var=cdr3_aa)
#' rep2DF(df2, clone_var=cdr3_nt) # translate 'cdr3_nt' to amino acid sequences
#'
#'
rep2DF <- function(rep, clone_var, clonotype_var) {

  if (!is.data.frame(rep) | length(rep)==1)

    return(clonalSeq2DF(rep))

  clonal_seq <- pull(rep, {{clone_var}})

  if (hasArg(clonotype_var)) {

    clonotype_seq <- pull(rep, {{clonotype_var}})

  } else {

    clonotype_seq <- translate(clonal_seq)

  }

  data.frame(clone=clonal_seq, clonotype=clonotype_seq)

}





#' list of repertoire to data frame
#'
#' @param rep_list list of character vectors or data frames
#' @param ... for data frames with more than one column, clonal sequence column name is needed. adding also clonotype column name is suggested
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' vec_list <- list(
#' A=c("ATG", "TTC", "TTT"),
#' B=c("TTT", "ATG", "ATG")
#' )
#' vec_list
#'
#' repList2DF(vec_list)
#'
#' df_list <- list(
#'   A=data.frame(nt=c("ATG", "TTC", "TTT"),
#'                aa=c("M", "F", "F")),
#'   B=data.frame(nt=c("TTT", "ATG", "ATG"),
#'                aa=c("F", "M", "M"))
#' )
#'
#' df_list
#'
#' repList2DF(df_list, nt)
#' repList2DF(df_list, nt, aa)
#'
repList2DF <- function(rep_list, ...) {

  lapply(rep_list, rep2DF, ...) %>%

  bind_rows(.id = "rid")

}



#' join groups of repertoire list into a dataframe
#'
#' @param group_list list-of-list of character vector or data frames
#' @param ... for data frames with more than one column, clonal sequence column name is needed. adding also clonotype column name is suggested
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' vec_list1 <- list(
#' A=c("ATG", "TTC", "TTT"),
#' B=c("TTT", "ATG", "ATG")
#' )
#' vec_list2 <- list(
#'   C=c("ATG", "TTC", "TTT"),
#'   D=c("TTT", "ATG", "ATG")
#' )
#'
#' grouped_rep_list <- list(group1=vec_list1,group2=vec_list2)
#'
#' groupList2DF(grouped_rep_list)
#'
groupList2DF <- function(group_list, ...) {

  lapply(group_list, repList2DF, ...) %>%

    bind_rows(.id = "gid")
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


