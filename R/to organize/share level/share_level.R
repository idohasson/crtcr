# group_share_level(clonotype_split(AA, ID))
group_share_level <- function(id_list, group_level=mean) {

  if (is_formula(group_level))

    group_level <- as_function(group_level)

  if (is_function(group_level)) {

    levels <- share_level(id_list)

    group_level <- group_level(levels)

  } else if (is_null(group_level)) {

    group_id <- unlist(id_list, use.names = FALSE)

    group_level <- share_level(group_id)

  } else stop("group_func must be either a function, formula or NULL")

  return(group_level)
}
