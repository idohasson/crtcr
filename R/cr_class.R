#' Sub-classification of public clonotype
#'
#' @param public_min
#' @param exclusive_min
#' @param share_tbl
#'
#' @return
#'
#' @importFrom dplyr case_when
#'
#' @export
#'
#' @examples
cr_class <- function(share, public_min=1L, exclusive_min=1L) { # vector
  # TODO: check input and generate sharing table if needed
  # if (max(n, na.rm=TRUE) > public_min) return("private") else
  # if (sum(n!=0, na.rm=TRUE) > exclusive_min)

  cr_index <- function(n) {
  #           numeric vector of the unique number of samples
  #            having a specific clonotype in every group
  (max(n, na.rm=TRUE) > public_min) + (sum(n!=0, na.rm=TRUE) > exclusive_min)
  #       can't be private                        multiple shared samples
  #       public clonotype                          inclusive clonotype
  }
  apply(share_tbl, 1, cr_index) %>%
  # private = 0 | exclusive = 1 | inclusive = 2
  {case_when(. == 0 ~ "private",
            . == 1 ~ "exclusive",
            . == 2 ~ "inclusive")}
}



# # CR-class for clonotype
# sharing_samples <- rowSums(clonotype_share_level, na.rm = TRUE)
# sharing_groups <- rowSums(clonotype_share_level != 0, na.rm = TRUE)
#
# is_private <- sharing_samples == 1
# is_public <- sharing_samples > 1
# is_exclusive <- is_public & (sharing_groups == 1)
# is_inclusive <- is_public & (sharing_groups > 1)
#
# clonotype_sharing <- list(sample=sharing_samples,
#                           group=sharing_groups)
#
# clonotype_cr_type <- list(private=is_private,
#                           public=is_public,
#                           exclusive=is_exclusive,
#                           inclusive=is_inclusive)
#
#
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
