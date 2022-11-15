#################### time series ####################

cr_time_series <- function(groups) {

  clonotype_count <- group_count(groups)

  gorup_layers <- seq(length(groups)-1) %>%

    lapply(function(i) clonotype_count[c(i, i + 1)]) %>%

    setNames(paste("layer", seq_along(.), sep = "")) %>%

    map_dfr(.id = "layers", .f = function(pair) {

      filter(pair, if_any(everything(), ~ . != 0)) %>% cr_type }) %>%

    column_to_rownames("layers")


  gorup_layers

}
