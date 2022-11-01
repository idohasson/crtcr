# bit_sharing
# bit_unique
# bitC <- function(x) noquote(vapply(as.double(x), function(x) { # split one double
#   b <- substr(as.character(rev(numToBits(x))), 2L, 2L)
#   paste0(c(b[1L], " ", b[2:12], " | ", b[13:64]), collapse = "")
# }, ""))
# bitC(17)

#' Label a clonotype list with the appropriate CR classifications.
#'
#' @description clonotypes are given a label depending on the CR-types that they are associated with.
#'
#' @param clonotype_list list of character vectors
#'
#' @return character vector named by colonotypes
#'
#' @export
#'
#' @examples
#'
#' rand_rep <- function(n, alphabet = LETTERS[1:10])
#'   sample(alphabet, n, replace = TRUE)
#'
#' rep_list <- replicate(3, rand_rep(rpois(1, 10)), simplify = FALSE)
#'
#' public_cr(rep_list)
#'
public_cr <- function(share_tbl) { # vector
  # The share_table function takes a list of clonotypes
  # and returns a table of clonotype frequencies.
  # tbl <- share_table(clonotype_list)
  # The cr_index function takes a vector of clonotype
  # frequencies and returns a numeric vector of the
  # unique number of samples having a specific clonotype
  # in every group.
  cr_index <- function(n)
    #     numeric vector of the unique number of samples
    #       having a specific clonotype in every group
    (max(n, na.rm = TRUE) > 1) + (sum(n != 0, na.rm = TRUE) > 1)
    #   can't be private             multiple shared samples
    #   public clonotype               inclusive clonotype

  # a numeric vector of the unique number of samples
  # having a specific clonotype in every group.
  cr_i <- apply(share_tbl, 1, cr_index)
  # assign a label to each value of cr_i
  # private = 0 | exclusive = 1 | inclusive = 2
  case_when(cr_i == 0 ~ "private",
            cr_i == 1 ~ "exclusive",
            cr_i == 2 ~ "inclusive")
}




# rand_group("df_list", n_sample = 10, 1000, 3) %>%
#   cr_table("aa", list(1:5, 6:10))
#
# cr_tbl <- cr_table(x, "nt", list(1:5, 6:10))
cr_table <- function(clonotype_list, ...) {

  if (every(clonotype_list, is.data.frame)) {

    share_tbl <- shared_population_clonotype(clonotype_list, ...)

  } else if (every(clonotype_list, is.character)) {

    share_tbl <- share_table(clonotype_list)

  } else {
    stop()
  }

  public_cr(share_tbl) %>%

    cbind.data.frame(share_tbl, cr=.)

}


factor_cr <- function(group_count) {

  compute_type <- function(tbl)
    # public clonotype    inclusive clonotype
    # can't be private    multiple shared samples
    (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
  # private = 0 | exclusive = 1 | inclusive = 2

  factor(compute_type(group_count),
         levels = c(0, 1, 2),
         labels = c("private", "exclusive", "inclusive"))
}


#' Split populations' clonotypes to their respective CR-subgroups.
#'
#' @description Make a list of the clonotypes based on the CR-types (private, exclusive & inclusive) that correspond to each one.
#'
#' @param clonotype_list a character vector list of the clonotypes found in each individual group
#'
#' @return list of distinct clonotype groups named by the corisponding CR-types: 'private', 'exclusive', 'inclusive'
#'
#' @export
#'
#' @examples
#'
#' clonotype_list <- replicate(3, rand_rep(rpois(1, 10)), simplify = FALSE)
#'
#' cr_seq(clonotype_list)
#'
cr_list <- function(clonotype_list, ...) {

  if (every(clonotype_list, is.data.frame)) {

    share_tbl <- shared_population_clonotype(clonotype_list, ...)

  } else if (every(clonotype_list, is.character)) {

    share_tbl <- share_table(clonotype_list)

  } else {
    stop()
  }

  split(rownames(share_tbl), factor_cr(share_tbl))

  # clonotype_list %>% # list of clonotypes (character vectors)
  #   # function that takes a list of clonotypes and returns a
  #   # named character vector of CR-types.
  #   public_cr() %>%
  #   # splits the named character vector into a list of character vectors.
  #   split(x = names(.))
}
