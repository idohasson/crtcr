


#' Join all repertoires' clonotypes data into one data frame
#'
#' @param ... repertoire group
#'
#' @return data frame
#'
#' @importFrom rlang list2
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr modify_depth map pluck
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'
#' @examples
#'
#' # generate random
#' g <- replicate(4, rand_group(), simplify = FALSE)
#'
#' group_join(g)
#'
group_join <- function(...) {

  rep <- list2(...) # TODO: dots_list
  # TODO check valid input
  # TODO make sure rep values are unique valid input
  if (is.vector(rep)) {

    if (is.vector(pluck(rep, 1))) {

      if (is.list(rep) & length(rep)==1)
        rep %<>% pluck(1)

      if (is.character(pluck(rep, 1, 1)))
        rep %<>% modify_depth(2, function(nt)
          cbind.data.frame(clone=nt,
                           clonotype=translate(nt)))

      if (is.data.frame(pluck(rep, 1, 1)))
        rep %<>% map(bind_rows, .id = "rep_id")
    }

    if(is.data.frame(pluck(rep, 1)))
      rep %<>% bind_rows(.id = "group")

  } else if (is.data.frame(rep)) {

    if (is.character(rep[1,3]) & ncol(rep)==3)
      rep %<>% mutate(clonotype=translate(pluck(3)))

  } else stop("Invalid input error")

  # # TODO check valid input
  # if (ncol(rep) == 4)
  #   colnames(rep) <- c("group", "rep_id", "clone", "clonotype")
  # else stop("ERROR")

 rep

}



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

#' convert list of nucleation sequences to data frame
#'
#' @param rep_list list
#' @param rep_names sample names
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' nt_list <- rand_group()
#'
#' group_df <- replist2DF(nt_list, rep_names=LETTERS[seq_along(nt_list)])
#'
#' head(group_df)
#'
replist2DF <- function(rep_list, rep_names) {

  if (!missing(rep_names))

    rep_list %<>% set_names(rep_names)

  map_dfr(rep_list, . %>%

  bind_cols(clone=., clonotype=translate(.)) %>%

  distinct, .id = "rid")

}








