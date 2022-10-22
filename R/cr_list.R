





# stopifnot(vec_depth(group_list)==3)

# clonotype_list <- group_list %>%
#   lapply(map, unique) %>%
#   simplify_all(.type = "character")
#
# clonotype_count <- clonotype_list %>%
#   map_dfr(vec_count, .id = "group") %>%
#   pivot_wider(names_from = "group",
#               values_from = "count",
#               values_fill = 0) %>%
#   column_to_rownames("key")
# # %>%
# #   split(apply(., 1, cr_index))
#
# clonotype_count
#
# apply(clonotype_count[-1], 1, cr_index)
#
#
#
#
# unique_clonotypes(group_list, c("A", "B"))

