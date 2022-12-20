
# add_cr_level(DF, "nt", "aa")
add_cr_level <- function(df, nt_field=1, aa_field=NULL) {

  nt <- field(df, nt_field)

  aa <- if (is_null(aa_field)) {translate(nt)} else {field(df, aa_field)}

  cr_seq <- vec_group_loc(aa)

  cr_list <- vec_chop(nt, cr_seq$loc)

  each_row_level <- function(x) rep_along(x, vec_unique_count(x))

  cr_levels <- lapply(cr_list, each_row_level)

  levels_vec <- list_unchop(cr_levels, indices = cr_seq$loc)

  vec_cbind(df, CRlevel=levels_vec)

  # vec_cbind(clonotype=aa, clone=nt, CRlevel=levels_vec)

}

