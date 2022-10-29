#'
#'
#'
#' #' Title
#' #'
#' #' @param input_list
#' #'
#' #' @return
#' #'
#' #' @export
#' #'
#' #' @examples
#' cr_source <- function(input_list) {
#'   if (is.null(names(input_list))) {
#'     names(input_list) <- letters[seq_along(input_list)]
#'   }
#'   input_list <- lapply(input_list, unique)
#'
#'   df <- lapply(input_list, function(aa) data.frame(from = aa))
#'   df <- bind_rows(df, .id = "to")
#'   df <- df[2:1]
#'
#'   sectors <- reduce(input_list, union)
#'   sectors <- c(sectors, rev(names(input_list)))
#'
#'   nt_n <- n_distinct(df$from)
#'   nt_w <- rep(1, nt_n) / nt_n * 2/3
#'
#'   g_n <- n_distinct(df$to)
#'   g_w <- rep(1, g_n) / g_n * 1/3
#'   sw <- c(nt_w, g_w)
#'
#'   circlize::circos.par(start.degree = 90,
#'              clock.wise <- FALSE,
#'              RESET = TRUE)
#'
#'   circlize::circos.initialize(sectors, xlim = c(0, 1))
#'   circlize::circos.initialize(sectors, xlim = c(0, 1), sector.width = sw)
#'
#'   circlize::circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#'     circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index)
#'   })
#'
#'   color_scale <- adjustcolor(seq_along(df), alpha.f=.6)
#'   names(color_scale) <- names(input_list)
#'
#'   link_df <- arrange_links_evenly(df)
#'   link_df["color"] <- color_scale[link_df$sector2]
#'
#'   for (i in seq(nrow(link_df))) {
#'     s1 = df$from[i]
#'     s2 = df$to[i]
#'     circlize::circos.link(link_df[i, "sector1"],
#'                 c(0,1),
#'                 link_df[i, "sector2"],
#'                 link_df[i, c("pos1", "pos2")],
#'                 col = link_df[i, "color"])
#'   }
#'
#'   circlize::circos.clear()
#' }
