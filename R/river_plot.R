# library("dplyr")
# library("ggplot2")
# library("ggforce")
#
# library("tidyr")
# library("vctrs")
# library("purrr")
#
# A_col <- "darkorchid1"
# B_col <- "darkorange1"
# C_col <- "skyblue1"
# alpha <- 0.7 # transparency value
# fct_levels <- c("A","C","B")
#
# # dat <- cr_prop(l1, l2, l3) %>%
# #   # proportions("type")
# #   proportions("variable")
#
# dat_ggforce <- as.data.frame(dat)  %>%
#   gather_set_data(1:2) %>%        # <- ggforce helper function
#   arrange(x,variable,desc(type))
# #
# # ggplot(dat_ggforce, aes(x = x, id = id, split = y, value = Freq)) +
# #   geom_parallel_sets(aes(fill = variable), alpha = alpha, axis.width = 0.2,
# #                      n=100, strength = 0.5) +
# #   geom_parallel_sets_axes(axis.width = 0.25, fill = "gray95",
# #                           color = "gray80", size = 0.15) +
# #   geom_parallel_sets_labels(colour = 'gray35', size = 4.5, angle = 0, fontface="bold") +
# #   scale_fill_manual(values  = c(A_col, B_col, C_col)) +
# #   scale_color_manual(values = c(A_col, B_col, C_col)) +
# #   theme_minimal() +
# #   theme(
# #     legend.position = "none",
# #     panel.grid.major = element_blank(),
# #     panel.grid.minor = element_blank(),
# #     axis.text.y = element_blank(),
# #     axis.text.x = element_text(size = 20, face = "bold"),
# #     axis.title.x  = element_blank()
# #   )
# #
