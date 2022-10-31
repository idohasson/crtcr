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
cr_seq <- function(clonotype_list) {

  clonotype_list %>% # list of clonotypes (character vectors)
    # function that takes a list of clonotypes and returns a
    # named character vector of CR-types.
    public_cr() %>%
    # splits the named character vector into a list of character vectors.
    split(x = names(.))
}
