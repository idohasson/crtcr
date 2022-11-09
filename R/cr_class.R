#' Sub-classification of public clonotype
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
#' cr_class(tbl)
#'
cr_class <- function(group_count, margin=1, min_freq=1, public_min=2, exclusive_max=1) {

  # computes the CR-class of each clonotype by its frequency in each group of repertoires.
  compute_type <- function(x) {
    # public clonotype    inclusive clonotype
    i <- x >= min_freq
    # can't be private    multiple shared samples
    # (sum(x[i]) >= public_min) + (sum(i) <= exclusive_max)
    (p <- sum(x[i]) >= public_min) + (p & sum(i) <= exclusive_max)
    # private = 0 | exclusive = 1 | inclusive = 2
  }

  cr_index <-

    if (is.vector(group_count)) # checking whether the input is a vector.
      # If it is, then it’s a simple count and we can just use the compute_type function.
      compute_type(group_count)
    # If the input is not a vector, then we need to check whether the object has dimensions (it’s a array / matrix / data frame).
    else if (!is.null(dim(group_count)))
      # If it is, then we need to apply the compute_type function to each row.
      apply(group_count, margin, compute_type)
    # input is not a vector, matrix, or data frame, then we need to stop because we don’t know how to handle it.
    else stop("")

  dplyr::case_when(
    # checking if the cr_index is equal to 2.
    cr_index==2 ~ "exclusive", # If so, it returns the string “exclusive”.
    # checking if the cr_index is equal to 1.
    cr_index==1 ~ "inclusive", # If so, it returns the string “inclusive”.
    # checking if the cr_index is equal to 0.
    cr_index==0 ~ "private", # If so, it returns the string “private”.
    # checking if the cr_index is not equal to 2, 1, or 0.
    TRUE ~ NA_character_ # If so, it returns the string “NA”.
  )

  # factor(cr_index,
  #        levels = c(0, 1, 2),
  #        # labels = c("private", "exclusive", "inclusive"))
  #        labels = c("private", "inclusive", "exclusive"))
}

min_freq=1; public_min=2; exclusive_max=1
compute_type <- function(x) {
  # public clonotype    inclusive clonotype
  i <- x >= min_freq
  # can't be private    multiple shared samples
  # (sum(x[i]) >= public_min) + (sum(i) <= exclusive_max)
  (p <- sum(x[i]) >= public_min) + (p & sum(i) <= exclusive_max)
  # private = 0 | exclusive = 1 | inclusive = 2
}

# whole numbers
# library("magrittr")
f <- function(x) {
  # if(!any(i <- (x >= min_freq & !is.na(x)), na.rm = TRUE)) return(NA)
  # any(i <- (x >= min_freq & !is.na(x)), na.rm = TRUE)
  # i <- x >= min_freq & !is.na(x)
  i <- x >= min_freq

  is.not.empty <- any(i, na.rm = TRUE)
  is.public <- sum(x[i], na.rm = TRUE) >= public_min # FALSE is private
  is.exclusive <- sum(i, na.rm = TRUE) <= exclusive_max # FALSE is inclusive

  dplyr::case_when(
    is.public & is.exclusive ~ "exclusive",
    is.public ~ "inclusive",
    is.not.empty ~ "private",
    TRUE ~ NA_character_
  )

}

min_freq=1; public_min=2; exclusive_max=1
c(0,0,0,0) %>% f()
c(0,0,0,NA) %>% f()
c(NA,NA,NA,NA) %>% f()
c(0,1,0,0) %>% f()
c(0,1,0,NA) %>% f()
c(NA,1,NA,NA) %>% f()
c(0,1,0,1) %>% f()
c(NA,1,NA,1) %>% f()
c(0,0,0,2) %>% f()
c(0,NA,0,2) %>% f()
c(NA,NA,NA,2) %>% f()
c(0,0,1,2) %>% f()
# cr_class(min_freq=1, public_min=2, exclusive_max=1)
min_freq=.1; public_min=.25; exclusive_max=1
c(0,0,0,0) %>% f()
c(0,0,0,NA) %>% f()
c(NA,NA,NA,NA) %>% f()
c(0,.1,0,0) %>% f()
c(0,.1,0,NA) %>% f()
c(NA,.1,NA,NA) %>% f()
c(0,.1,0,.1) %>% f()
c(NA,.1,NA,.1) %>% f()
c(0,0,0,.2) %>% f()
c(0,NA,0,.2) %>% f()
c(NA,NA,NA,.2) %>% f()
c(0,0,.1,.2) %>% f()





















