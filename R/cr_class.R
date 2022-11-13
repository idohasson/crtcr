
#' CR-class of a clonotype by its share-level among groups.
#'
#' @describeIn
#'
#' given clonotype frequencies in each group, calculate its convergent
#' recombination public classifications based on various conditions.
#'
#' @param shared numerical vector
#' @param min_shared minimum numerical value
#' @param min_public minimum sum from all frequencies having a minimal value to be considered as public clonotype
#' @param max_exclusive maximal number of public clonotypes groups to be considered as public exclusive. o.w. inclusive.
#'
#' @return character object of "private" / "inclusive" / "exclusive" / NA_character_
#' @export
#'
#' @examples
#'
#' cr_class(c(1,0,1,0)) # public inclusive
#'
#' cr_class(c(2,0,0,0)) # public exclusive
#'
#' cr_class(c(1,0,0,0)) # private
#'
cr_class <- function(shared, min_shared=1, min_public=2, max_exclusive=1) {

  is_shared <- shared >= min_shared
  # returns TRUE if the vector of frequencies has at least one non-missing value.
  any_shared <- any(is_shared, na.rm = TRUE)
  # returns TRUE if the sum of frequencies is greater than or equal to the minimum public frequency.
  public <- sum(shared[is_shared], na.rm = TRUE) >= min_public
  # returns TRUE if the number of groups having minimal frequency value is less than or equal to the maximum exclusive frequency.
  exclusive <- public & sum(is_shared, na.rm = TRUE) <= max_exclusive
  # return the appropriate class label based on the values in the previous lines.
  case_when(
    exclusive ~ "exclusive",
    public ~ "inclusive",
    any_shared ~ "private",
    TRUE ~ NA_character_
  ) # returns NA if none of the above conditions are met.
}

#' # tidyr::gather(group, share_level, -clonotype, na.rm = TRUE)
#'
#' cr_table <- function(gruop_list) {
#'
#'   gruop_list %>%
#'
#'     map_dfr(share.level, .id = "gid") %>%
#'
#'     group_by(clonotype) %>%
#'
#'     summarise(cr=cr_freq_class(share))
#' }
#'
#'
#'
#' #' Sub-classification of public clonotype
#' #'
#' #' @description computes the CR-class of each clonotype by its frequency in each group of repertoires.
#' #'
#' #'
#' #' @param group_count numerical vector / or multiple dimensional data-type (data frame / matrix / array) of number of colonotype in each group
#' #' @param margin the dimension to apply
#' #' @param min_freq minimal value per group
#' #' @param public_min minimum total sum of the
#' #' @param exclusive_max maximum number of groups that could be conciser exclusive
#' #'
#' #' @return character vector
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' df_lists <- replicate(4, rand_group(), FALSE)
#' #' rand_df <- group_join(df_lists)
#' #' tbl <- share_table(rand_df, "clonotype", "group", "rep_id")
#' #' cr_class(head(tbl, 5))
#' #'
#' cr_class <- function(group_count, margin=1, min_freq=1, public_min=2, exclusive_max=1) {
#'
#'   if (is.vector(group_count)) # checking whether the input is a vector.
#'     # If it is, then it’s a simple count and we can just use the compute_type function.
#'     cr_freq_class(group_count, min_freq, public_min, exclusive_max)
#'     # If the input is not a vector, then we need to check whether the object has dimensions (it’s a array / matrix / data frame).
#'   else if (!is.null(dim(group_count)))
#'     # If it is, then we need to apply the compute_type function to each row.
#'     apply(group_count, margin, cr_freq_class, min_freq, public_min, exclusive_max)
#'     # input is not a vector, matrix, or data frame, then we need to stop because we don’t know how to handle it.
#'   else stop("Invalid input")
#'
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
#'
#'
#'
#'
#'
#'
#'
