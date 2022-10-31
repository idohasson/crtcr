


#' Title
#'
#' @param input_list
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' g <- list(rep1=rand_rep_vec(seq_type = "nt", 7, rep(5, 7)),
#'           rep2=rand_rep_vec(seq_type = "nt", 9, rep(5, 9)))
#'
cr_source <- function(input_list) {

  if (!rlang::is_named(input_list)) names(input_list) <- as.character(seq_along(input_list))

  # input_list <- lapply(input_list, unique)

  # sectors <- c("source", "groups")

  # sectors <- purrr::reduce(input_list, union) %>%
  #
  #   append(rev(names(input_list)))
  # nt_n <- n_distinct(df$from)
  # nt_w <- rep(1, nt_n) / nt_n * 2/3
  #




  seq <- Reduce(union, input_list)

  sectors <- c(rev(seq), names(input_list))

  axes <- matrix(data = rep(1, length(seq)), dimnames = list(seq)) %>%

    rbind(as.matrix(sapply(g, n_distinct))) %>% cbind(0, .)

  sw <- c(n_distinct(unlist(input_list)) %>% rep(2/3 / ., .),
          proportions(sapply(input_list, n_distinct)) / 3)

  n <- nchar(seq[1])


  circlize::circos.par(RESET = TRUE,
    cell.padding = c(0, 0, 0, 0),
    track.margin = c(0.005, 0.005),
    # start.degree = 0,
    # gap.degree = c(rep(2, n-1), 5, 5),
    start.degree = 90,
    clock.wise = FALSE,
    points.overflow.warning = FALSE
    # start.degree = 90,
             # clock.wise = FALSE,
             # RESET = TRUE
    )

  circlize::circos.initialize(sectors, xlim = axes, sector.width = sw)

  # circos.track(ylim = c(0, 1), track.height = 0.6, panel.fun = function(x, y) {
  #   x = seq(0.2, 0.8, by = 0.2)
  #   y = seq(0.2, 0.8, by = 0.2)
  #
  #   circos.segments(x, 0.1, x, 0.9)
  #   circos.segments(0.1, y, 0.9, y)
  # })



  circlize::circos.track(sectors=seq, ylim = c(0, n), bg.border = NA,
                         track.height = 2/3, panel.fun = function(x, y) {

    nt_col <- strsplit(get.cell.meta.data("sector.index"), split = "")[[1]] %>%

      stringr::str_replace_all(c("A" = "tomato",
                                 "G" = "khaki1",
                                 "T" = "dodgerblue",
                                 "C" = "palegreen")) %>% rev()

    circlize::circos.rect(.25, 1:n - 1, rep(.75, n), 1:n,
                col = nt_col, border = adjustcolor("black", alpha.f=.1))
    # circlize::circos.rect(get.cell.meta.data("xlim")[1], .15,
    #                       get.cell.meta.data("xlim")[2], .85,
    #             col = "azure3", border = adjustcolor("black", alpha.f=.1))



    # circlize::circos.text(circlize::CELL_META$xcenter, circlize::CELL_META$ycenter, circlize::CELL_META$sector.index)
  })



  colors <- c(adjustcolor("red", alpha.f=.6),
              adjustcolor("blue", alpha.f=.6))

  if (length(input_list)>2) {
    colors <- c(colors, rand_color(length(input_list)-2,
                                   luminosity = "random",
                                   transparency = .6))

      }

  link_df <- input_list %>%

    lapply(function(nt) data.frame(from = nt)) %>%

    bind_rows(.id = "to") %>% rev() %>%

    circlize::arrange_links_evenly() %>%

    mutate(color = as.character(factor(sector2, labels = colors)))


  for (i in seq(nrow(link_df))) {
    circlize::circos.link(link_df[i, "sector1"], c(.15, .85),
                          link_df[i, "sector2"], link_df[i, c("pos1", "pos2")],
                          col = link_df[i, "color"],
                          border = adjustcolor("black", alpha.f=.3),lwd = .5)

  }
  circlize::circos.clear()
}


#
# g<-rand_group(rep_type = "nt_vector", n_sample = 2, seq_n = 10, seq_len=rep(13,10))



