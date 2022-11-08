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

  compute_type <- function(x) {
    # public clonotype    inclusive clonotype
    # can't be private    multiple shared samples
    i <- x >= min_freq
    # (sum(x[i]) >= public_min) + (sum(i) <= exclusive_max)
    (p <- sum(x[i]) >= public_min) + (p & sum(i) <= exclusive_max)
    # private = 0 | exclusive = 1 | inclusive = 2
  }

  cr_index <- if (is.vector(group_count))
    compute_type(group_count)
  else if (!is.null(dim(group_count)))
    apply(group_count, margin, compute_type)
  else stop("")

  case_when(
    cr_index==2 ~ "exclusive",
    cr_index==1 ~ "inclusive",
    cr_index==0 ~ "private",
    TRUE ~ NA_character_
  )

  # factor(cr_index,
  #        levels = c(0, 1, 2),
  #        # labels = c("private", "exclusive", "inclusive"))
  #        labels = c("private", "inclusive", "exclusive"))
}

# sharing_samples <- rowSums(clonal_seq_share_level, na.rm = TRUE)
# sharing_groups <- rowSums(clonal_seq_share_level != 0, na.rm = TRUE)
#
# is_private <- sharing_samples == 1
# is_public <- sharing_samples > 1
# is_exclusive <- is_public & (sharing_groups == 1)
# is_inclusive <- is_public & (sharing_groups > 1)
#
# clone_sharing <- list(sample=sharing_samples,
#                       group=sharing_groups)
#
# clone_cr_type <- list(private=is_private,
#                       public=is_public,
#                       exclusive=is_exclusive,
#                       inclusive=is_inclusive)
#
#
# share_ratio <- clonotype_sharing$sample/clonotype_sharing$group
# clonotype_cr_ratio <- lapply(clonotype_cr_type, function(x) share_ratio[x])
# print(sapply(clonotype_cr_ratio, mean))
#
# share_ratio <- clone_sharing$sample/clone_sharing$group
# clone_cr_ratio <- lapply(clone_cr_type, function(x) share_ratio[x])
# print(sapply(clone_cr_ratio, mean))
