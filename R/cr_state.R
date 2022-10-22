
cr_index <- function(n){
  # numeric vector of the unique number of samples
  # having a specific clonotype in every group
  (max(n, na.rm = TRUE) > 1) + (sum(n != 0, na.rm = TRUE) > 1)
  # public clonotype    inclusive clonotype
  # can't be private    multiple shared samples
}

cr_type <- function(i) {
  case_when(
    i == 0 ~ "private",
    i == 1 ~ "exclusive",
    i == 2 ~ "inclusive"
  )
}



cr_type <- function() {

  compute_type <- function(tbl)
    # public clonotype    inclusive clonotype
    # can't be private    multiple shared samples
    (rowSums(tbl) > 1) + (rowSums(tbl != 0) > 1)
  # private = 0 | exclusive = 1 | inclusive = 2
  factor(compute_type(group_count),
         levels = c(0, 1, 2),
         labels = c("private", "exclusive", "inclusive"))
}



lapply(dfl, pull, "aaSeqCDR3") %>%

  share_level_per_gruop %>%

  apply(1, cr_index) %>%

  split(cr_type(.)) %>%

  modify(names)

# cr_type <- function(l1, l2) {
#
#   ! every(l1, is.character)
#
#   modify_if(l1, is.character, vec_count)
#   # l <- list(l1, l2)
# }
#
#








# gl <- group_gen(3, 100)
# gl <- lapply(gl, function(l) lapply(l, unique))
# gl <- lapply(gl, unlist)

# gc <- cr_type(gl)
# cr_type(gl)

# %>%
#   column_to_rownames("clonotype")
# vec_count(gc$key, gc$count)


