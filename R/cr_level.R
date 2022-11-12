

df <- rep2DF(r)


cr_level(group_df)

df %>%
  group_by(clonotype, .add = TRUE) %>%
  summarise(n_distinct(clone))




# tarnslate_clone <- function(data, clone_var) {
#   # rand_df[-4] %>% tarnslate_clone(clone)
#   mutate(data, across({{clone_var}}, translate, .names = "clonotype"))
# }
#
# add_cr_level <- function(data, clone_var, clonotype_var) {
#   # rand_df %>% add_cr_level(clone, clonotype)
#   # rand_df[-4] %>% add_cr_level(clone)
#   # rand_df %>% add_cr_level(clone)
#
#   if (quo_is_missing(enquo(clonotype_var))) {
#     data %<>% tarnslate_clone({{clone_var}})
#     clonotype_var <- quo(clonotype)
#   }
#
#   data %>% group_by({{clonotype_var}}, .add = TRUE) %>%
#
#     mutate(across({{clone_var}},  n_distinct, .names = "CR_level"))
#
# }
#
cr_level <- function(data, clone_var, clonotype_var) {
  # EXAMPLE:
  # rand_df %>% group_by(group, rep_id) %>% cr_level(clone, clonotype)
  # rand_df[-4] %>% group_by(group, rep_id) %>% cr_level(clone)

  if (missing(clone_var))
    clone_var <- as.name(nth(names(data), -2))

  if (missing(clonotype_var))
    clonotype_var <- as.name(nth(names(data), -1))

  data %>% group_by({{clonotype_var}}, .add = TRUE) %>%

    summarise(across({{clone_var}}, n_distinct, .names = "CR_level"), .groups = "drop_last")

}
