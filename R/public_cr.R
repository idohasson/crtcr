
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
public_cr <- function(clonotype_list) {
  # The share_table function takes a list of clonotypes
  # and returns a table of clonotype frequencies.
  tbl <- share_table(clonotype_list)
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
  cr_i <- apply(tbl, 1, cr_index)
  # assign a label to each value of cr_i
  # private = 0 | exclusive = 1 | inclusive = 2
  case_when(cr_i == 0 ~ "private",
            cr_i == 1 ~ "exclusive",
            cr_i == 2 ~ "inclusive") %>%

    setNames(rownames(tbl))
}





# public_cr <- function(group_count) {
#   compute_type <- function(tbl)
#     # public clonotype    inclusive clonotype
#     # can't be private    multiple shared samples
#     (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
#   # private = 0 | exclusive = 1 | inclusive = 2
#   # i <- compute_type(group_count)
#   # case_when(
#   #   i == 0 ~ "private",
#   #   i == 1 ~ "exclusive",
#   #   i == 2 ~ "inclusive")
#
#   factor(compute_type(group_count),
#          levels = c(0, 1, 2),
#          labels = c("private", "exclusive", "inclusive"))
# }
