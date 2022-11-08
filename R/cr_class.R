#' #' Sub-classification of public clonotype
#' #'
#' #' @param public_min
#' #' @param exclusive_min
#' #' @param share_tbl
#' #'
#' #' @return
#' #'
#' #' @importFrom dplyr case_when
#' #'
#' #' @export
#' #'
#' #' @examples
#' cr_class <- function(share_tbl, public_min=1L, exclusive_min=1L) { # vector
#'   # TODO: check input and generate sharing table if needed
#'   # if (max(n, na.rm=TRUE) > public_min) return("private") else
#'   # if (sum(n!=0, na.rm=TRUE) > exclusive_min)
#'
#'   cr_index <- function(n) {
#'   #           numeric vector of the unique number of samples
#'   #            having a specific clonotype in every group
#'   (max(n, na.rm=TRUE) > public_min) + (sum(n!=0, na.rm=TRUE) > exclusive_min)
#'   #       can't be private                        multiple shared samples
#'   #       public clonotype                          inclusive clonotype
#'   }
#'   i <- apply(share_tbl, 1, cr_index)
#'   # private = 0 | exclusive = 1 | inclusive = 2
#'   dplyr::case_when(i == 0 ~ "private",
#'               i == 1 ~ "exclusive",
#'               i == 2 ~ "inclusive",
#'               TRUE ~ NA)
#' }
#'
#' @param share_table
#' @param public_min
#' @param exclusive_min


cr_class <- function(share_table, public_min=1L, exclusive_min=1L) {

  sharing_samples <- rowSums(share_table, na.rm = TRUE)
  sharing_groups <- rowSums(share_table != 0, na.rm = TRUE)

  ratio <- sharing_samples/sharing_groups
  i <- (sharing_samples<=public_min) + (sharing_groups<=exclusive_min)
  i[is.na(ratio)] <- NA

  ratio %>% setNames(
    case_when(i==0 ~ "private",
              i==1 ~ "exclusive",
              i==2 ~ "inclusive",
              is.na(i) ~ NA_character_)
  )
  # i[1:10]
  # is_private <- sharing_samples == 1
  # is_exclusive <- is_public & (sharing_groups == 1)
  # is_inclusive <- is_public & (sharing_groups > 1)
  # case_when(sharing_samples<=public_min ~ "private",
  #           sharing_groups<=exclusive_min ~ "exclusive",
  #           TRUE ~ "inclusive")

}

g <- replicate(4, rand_group(), simplify = FALSE)
group_df <- group_join(g)
share_table <- share_table(group_df, "clonotype", "group", "rep_id")
share_table[6] <- NA
share_table[1:10,]
cr_class(share_table)[1:10]



sharing_samples <- rowSums(share_table, na.rm = TRUE)
sharing_groups <- rowSums(share_table != 0, na.rm = TRUE)
cr_class(share_table)
# cr_vec <- cr_class(share_table)

is_private <- sharing_samples == 1
is_public <- sharing_samples > 1
is_exclusive <- is_public & (sharing_groups == 1)
is_inclusive <- is_public & (sharing_groups > 1)

clonotype_sharing <- list(sample=sharing_samples,
                          group=sharing_groups)

clonotype_cr_type <- list(private=is_private,
                          public=is_public,
                          exclusive=is_exclusive,
                          inclusive=is_inclusive)
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
