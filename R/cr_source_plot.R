nt <- c("A", "G", "C", "T")
seq_len <- 3
seq_n <- c(12, 7)

clone1 <- replicate(seq_n[1], paste(sample(c("A", "G", "C", "T"), seq_len, replace = TRUE), collapse = ''))
clone2 <- replicate(seq_n[2], paste(sample(c("A", "G", "C", "T"), seq_len, replace = TRUE), collapse = ''))

input_list <- list(g1 = clone1, g2 = clone2)
df <- map_dfr(input_list, vec_count, .id = "group") %>%
  spread(group, count, 0)
row.names(df) <- df$key
df <- df[-1]
df <- df[order(df[1]),]


sectors <- unlist(dimnames(df))
circos.initialize(sectors, xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index)
})


color_scale <- adjustcolor(seq_along(df), alpha.f=.6)
names(color_scale) <- names(df)
mat <- adjacencyMatrix2List(as.matrix(df))
link_df <- arrange_links_evenly(mat[c("from", "to")])
link_df <- mutate(link_df, color = color_scale[sector2])

for(i in seq_len(nrow(link_df))) {
  s1 = mat$from[i]
  s2 = mat$to[i]
  circos.link(link_df[i, "sector1"], c(0,1),
              link_df[i, "sector2"], link_df[i, c("pos1", "pos2")],
              col = link_df[i, "color"])
}
circos.clear()


ntseq <- c(clone1, clone2)
n <- length(ntseq)
len <- nchar(max(ntseq))

aa_name <- "aa"
sourse_name <- c("A", "B")

names(ntseq) <- c(paste0(sourse_name[1], seq(length(clone1))),
                  paste0(sourse_name[2], seq(length(clone2))))
sectors <- c(names(ntseq), aa_name)
# Create x-axis range for each sector, 0-1 for each
axes <- matrix(rep(c(0, 1), each = n), ncol = 2)
axes <- rbind(axes, c(0, n))
rownames(axes) <- sectors
# circos.clear()



cr_source <- function(clone1, clone2, sectors, n, axes) {



  # Set circos parameters
  circos.par$cell.padding <- c(0, 0, 0, 0)
  circos.par$track.margin <- c(0.005, 0.005)
  circos.par$start.degree <- 0
  circos.par$gap.degree <- c(rep(2, n-1), 5, 5)
  circos.par$start.degree <- 90
  circos.par$clock.wise <- FALSE
  circos.par$points.overflow.warning <- FALSE
  # Initiate circos graph
  circos.initialize(sectors, xlim = axes, sector.width = c(rep(1, n), n/2))

  circos.track(sectors[-length(sectors)], x=(n-1):0, ylim=c(0, 1), bg.border = NA,
               track.height = mm_h(1), panel.fun = function(x, y) {
                 print(get.cell.meta.data("xlim"))
                 # Links track
                 to <- ifelse(x <= length(clone1), x + length(clone2), x - length(clone1))
                 to <- to * max(axes[aa_name,]) / n

                 circos.link(get.cell.meta.data("sector.index"), c(.15, .85), "aa", c(to-1, to))

                 # i <- get.cell.meta.data("sector.index")
                 # col1 <- adjustcolor("red", alpha.f=.6)
                 # col2 <- adjustcolor("blue", alpha.f=.6)
                 # link_col <- ifelse(x <= length(clone1), col1, col2)


                 # circos.link(i, c(.15, .85), aa_name, c(to-1, to), col = link_col,
                 #             border = adjustcolor("black", alpha.f=.3))

               })

  circos.clear()
}
cr_source(clone1, clone2, sectors, n, axes)
