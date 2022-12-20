# share_level(ID)
# share_level(clonotype_split(AA, ID))
share_level <- function(id, ...) {

  id_list <- dots_splice(id, ...)

  vapply(id_list, vec_unique_count, integer(1))

}

share_level <- function(aa, id) {
  vapply(vec_split(id, aa)$val, vec_unique_count, numeric(1))
}
