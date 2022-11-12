
# tidyr::gather(group, share_level, -clonotype, na.rm = TRUE)

cr_table <- function(gruop_list) {

  gruop_list %>%

    map_dfr(share.level, .id = "gid") %>%

    group_by(clonotype) %>%

    summarise(cr=cr_freq_class(share))
}




# pl <- list(human=gl1, mouse=gl2) %>%
#
#   map(set_names, c("cancer", "control"))
#
#
# pl %>% map_dfr(cr_table, .id = "pid") %>%
#
#   spread(pid, cr)












#
# gl %>%
#
#   map_dfr(share.level, .id = "gid")
#
# cr_table(gl)


#' Sub-classification of public clonotype
#'
#' @description computes the CR-class of each clonotype by its frequency in each group of repertoires.
#'
#'
#' @param group_count numerical vector / or multiple dimensional data-type (data frame / matrix / array) of number of colonotype in each group
#' @param margin the dimension to apply
#' @param min_freq minimal value per group
#' @param public_min minimum total sum of the
#' @param exclusive_max maximum number of groups that could be conciser exclusive
#'
#' @return character vector
#' @export
#'
#' @examples
#'
#' df_lists <- replicate(4, rand_group(), FALSE)
#' rand_df <- group_join(df_lists)
#' tbl <- share_table(rand_df, "clonotype", "group", "rep_id")
#' cr_class(head(tbl, 5))
#'
cr_class <- function(group_count, margin=1, min_freq=1, public_min=2, exclusive_max=1) {

  if (is.vector(group_count)) # checking whether the input is a vector.
    # If it is, then it’s a simple count and we can just use the compute_type function.
    cr_freq_class(group_count, min_freq, public_min, exclusive_max)
    # If the input is not a vector, then we need to check whether the object has dimensions (it’s a array / matrix / data frame).
  else if (!is.null(dim(group_count)))
    # If it is, then we need to apply the compute_type function to each row.
    apply(group_count, margin, cr_freq_class, min_freq, public_min, exclusive_max)
    # input is not a vector, matrix, or data frame, then we need to stop because we don’t know how to handle it.
  else stop("Invalid input")

}

#' function that takes a vector of frequencies and returns a character vector of class labels.
#'
#' @describeIn
#'
#' given clonotype frequencies in each group, calculate its convergent
#' recombination public classifications based on various conditions.
#'
#' @param c_freq numerical vector
#' @param min_freq minimum numerical value
#' @param public_min minimum sum from all frequencies having a minimal value to be considered as public clonotype
#' @param exclusive_max maximal number of public clonotypes groups to be considered as public exclusive. o.w. inclusive.
#'
#' @return character
#' @export
#'
#' @examples
#'
#' cr_freq_class(c(1,0,1,0)) # public inclusive
#' cr_freq_class(c(2,0,0,0)) # public exclusive
#' cr_freq_class(c(1,0,0,0)) # private
#'
cr_freq_class <- function(c_freq, min_freq=1, public_min=2, exclusive_max=1) {

  freq <- c_freq >= min_freq
  # returns TRUE if the vector of frequencies has at least one non-missing value.
  is.not.empty <- any(freq, na.rm = TRUE)
  # returns TRUE if the sum of frequencies is greater than or equal to the minimum public frequency.
  is.public <- sum(c_freq[freq], na.rm = TRUE) >= public_min
  # returns TRUE if the number of groups having minimal frequency value is less than or equal to the maximum exclusive frequency.
  is.exclusive <- sum(freq, na.rm = TRUE) <= exclusive_max
  # return the appropriate class label based on the values in the previous lines.
  dplyr::case_when(
    is.public & is.exclusive ~ "exclusive",
    is.public ~ "inclusive",
    is.not.empty ~ "private",
    TRUE ~ NA_character_ # returns NA if none of the above conditions are met.
  )
}

#TODO: update return

#' logical check if a frequencies vector is considered public with optional modifications
#'
#' @param freq numerical vector
#' @param min_freq minimal value to be counted as repertoire having a clonotype
#' @param public_min minimal number of repertoire having a clonotype to be conciser public
#' @param share_level TRUE if frequencies represent share-level (number of repertoires having a clonotype in each group), and FALSE for CR-level representation (number of same-clonotype clone in a repertoire)
#'
#' @return TRUE if its public, FALSE if private, and NA if none of the frequencies are sufficient
#' @export
#'
#' @examples
#'
#' is_public(c(1, 0 , 2, 4), min_freq=3)
#'
is_public <- function(freq, min_freq=1L, public_min=2L, share_level=TRUE) {

  if (!share_level)

    return(ifelse(freq > min_freq, freq > public_min, NA))

  min_i <- freq >= min_freq

  if (!any(min_i, na.rm = TRUE))
    return(NA) # none fulfilled the criteria

  sum(freq[min_i], na.rm = TRUE) >= public_min

}


#TODO: check this!

#' logical check if a frequencies vector is considered public-exclusive with optional modifications
#'
#' @param freq freq >= min_freq
#' @param id same length vector to split the groups by
#' @param min_freq minimal value to be counted as repertoire having a clonotype
#' @param min_rep minimal number of repertoire having a clonotype to be conciser public
#' @param max_group upper limit to number of groups the could be considered exclusive
#'
#' @return TRUE for exclusive, FALSE for inclusive, and NA if non of the groups are public
#' @export
#'
#' @examples
#'
#' is_exclusive(c(1, 0 , 2, 4), gl(2,2), min_freq=2)
#'
is_exclusive <- function(freq, id, min_freq=1L, min_rep=1L, max_group=1L) {

  sub_public <- tapply(freq, id, is_public, min_freq, min_rep)

  if (all(is.na(sub_public))) return(NA)

  sum(sub_public, na.rm = TRUE) <= max_group

}

















