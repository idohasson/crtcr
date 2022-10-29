#' Title
#'
#' @param groups
#'
#' @return
#' @export
#'
#' @examples
cr_time_series <- function(groups) {

  clonotype_count <- group_count(groups)

  gorup_layers <- seq(length(groups)-1) %>%

    lapply(function(i) clonotype_count[c(i, i + 1)]) %>%

    setNames(paste("layer", seq_along(.), sep = "")) %>%

    map_dfr(.id = "layers", .f = function(pair) {

      filter(pair, if_any(everything(), ~ . != 0)) %>% cr_type }) %>%

    column_to_rownames("layers")

    # apply(1, table)

  gorup_layers

  # clonotype_count[c("group1", "group2")] %>%
  #   mutate(type = pair1) %>%
  #   filter(group1 != 0 | group2 != 0) %>%
  #   xtabs(formula = cbind(group1, group2) ~ type)
}







