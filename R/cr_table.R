library(vctrs)
library(purrr)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)









# df1 <- distinct(dfl[[1]], aaSeqCDR3, nSeqCDR3)
# right_join(t, df1, by = "aaSeqCDR3")

# l1 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l2 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l3 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# t1 <- cr_table(l1, l2, l3)
# # t1 <- cr_prop(l1, l2, l3)
# list(l1, l2, l3) %>%
#   setNames(LETTERS[1:3]) %>%
#   map(flatten_chr) %>%
#   map_dfr(vec_count, .id = "group")
#
# # t1 %>% mutate(avg = rownames(.))
#
#
#
# l4 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l5 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# l6 <- sapply(rpois(rpois(1, 10), 10), rand_aa)
# t2 <- cr_table(l4, l5, l6)
# # t2 <- cr_prop(l4, l5, l6)
#
# t1 %>% head
#
# t2 %>% head
# l1
# t <- cr_table(l1, l2) %>%
#   tibble::rownames_to_column(var = "aaSeqCDR3")



# population_sharing <- list(l4, l5, l6) %>%
#   lapply(map, .f = vec_unique) %>%
#   lapply(flatten_chr) %>%
#   set_names(LETTERS[seq_along(.)]) %>%
#   map_dfr(vec_count, .id = "group") %>%
#   pivot_wider(names_from = group,
#               values_from = count,
#               values_fill = 0)


# state <- cr_type(population_sharing[-1])
# state <- data.frame(aa = population_sharing$key, type = state) %>% head
# # t <- cbind(population_sharing[1], state)
#
# cr.tbl <- cr_lvl.list.character(c(l1, l2, l3, l4, l5, l6))
# avg_cr <- apply(cr.tbl[-1], 1, average_cr)
# avg_cr <- data.frame(cr.tbl[1], avg_cr)



# cr.tbl <- cr_lvl.list.character(c(l4, l5, l6))
# avg_cr <- apply(cr.tbl[-1], 1, average_cr)
# avg_cr2 <- data.frame(cr.tbl[1], avg_cr)

# full_join(state, avg_cr, by = "aa")
# avg_cr[is.na(avg_cr)] <- 0



# reshape2::melt(t, id = "state")

# ?dplyr::transmute()
# ?truncate(population_sharing, )
#   cbind(population_sharing$key)

# my_rescale <- function(data, var, factor = 10) {
#   data %>% dplyr::mutate("{{ var }}" := {{ var }} / .env$factor)
#
#   # df %>% sample_n(10) %>% sharing_level_per_gruop %>% as.data.frame() %>% my_rescale("A")
# }

# a <- "A"

  # my_rescale(, variable.names("A"), 3)










