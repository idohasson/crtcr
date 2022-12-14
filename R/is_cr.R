
# l <- rand_clone_list()
# lu <- lapply(l, unique)
# s <- l[[1]]
# su <- unique(s)
all_cr <- function(nt) {

  if (is_character(nt))

    return(is_cr(nt))

  is_cr_vec <- is_cr.list(nt)

  if (which.max(is_cr_vec)==1)

    if (isTRUE(is_cr_vec[1]))

      return(TRUE)

  return(FALSE)

}

is_cr.list <- function(nt_list) {

  if (!is_bare_list(nt_list)) stop(
    "CR list must be a bare list"
  )

  vapply(nt_list, is_cr, logical(1), USE.NAMES = FALSE)

}

# is_cr(s)
# is_cr(su)
is_cr <- function(nt) {

  if (!is_character(nt)) stop(
    "Not a character vector"
  )

  unique_aa(nt)
  # no_nt_duplicates(nt) & same_unique_aa(nt)

}

unique_aa <- function(nt, aa=translate(nt[1])) {

  if (length(nt)==1)

    return(TRUE)

  for (nt_seq in nt[-1])

    if (aa != translate(nt_seq))

      return(FALSE)

  return(TRUE)

}

# sapply(x, no_nt_duplicates)
# sapply(unique_x, no_nt_duplicates)
no_nt_duplicates <- function(nt) {

  anyDuplicated(nt)==0

}

same_unique_aa <- function(nt) {

  if (length(nt)==1)

    return(TRUE)

  aa <- translate(nt)

  length(unique(aa)) == 1

}

