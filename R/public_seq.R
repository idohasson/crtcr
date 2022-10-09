# length.seqs <- 100 # length of DNA sequence
# nucl.freqs <- rep(1/4, 4) # nucleotide frequencies
# nucl <- c('a', 'c', 'g', 't') # A, C, G, T
#
# seqs <- sample(nucl, size = length.seqs, replace = TRUE, prob = nucl.freqs)
#
# bad_codons <- c("aga", "agg", "taa", "tag", "tga")
#
# regx <- paste0("(", paste(bad_codons, collapse = ")|("), ")")
#
# s <- paste(seqs, collapse = "")
#
# while( grepl(regx, s) ) {
#   s <- gsub(regx,
#             paste(sample(nucl, size = 3, replace = TRUE, prob = nucl.freqs), collapse = ""),
#             s)
# }
#
# s
#
# library(agvgd)
# package ‘segmented’
# package ‘grantham’
# package ‘seqinr’
# package ‘agvgd’


nt_list <- list(group1$BRCA1$nSeqCDR3, group1$BRCA2$nSeqCDR3, group1$BRCA3$nSeqCDR3)
lengths(nt_list)



aa_col_name <- "aaSeqCDR3"

vec_input <- list(group1$BRCA1$aaSeqCDR3, group1$BRCA2$aaSeqCDR3, group1$BRCA3$aaSeqCDR3)
vec_input <- setNames(vec_input, names(group1))

Map(c, vec_input)
lengths(vec_input)

is.pablic <- unlist(lapply(vec_input, unique), use.names = FALSE)
is.pablic <- table(aa_unique)>1

is.pablic[vec_input$BRCA1[1:10]]

?filter_if()

aa_check <- vec_input$BRCA1[1:10]

typeof(is_pablic[aa_check])

# aa_intersect <- function(pair) intersect(names(pair[[1]]), names(pair[[2]]))



aa_intersect <- function(pair) mapply(intersect, pair[1], pair[2])



aa <- aa_intersect(unname(vec_input)[1:2])[1:10]
group1$BRCA1[group1$BRCA1$aaSeqCDR3==aa,]


typeof(vec_input)

typeof

rep1 <- read_tsv(group1_paths[1], col_types = mixcr_column_type)

grouped_aa <- split(rep1$nSeqCDR3, rep1$aaSeqCDR3)
unique(rep1$aaSeqCDR3) %in% names(grouped_aa) %>% all

grouped_nt <- split(rep1$aaSeqCDR3, rep1$nSeqCDR3)
unique(rep1$nSeqCDR3) %in% names(grouped_nt) %>% all





# rep1 %>% select(cdr3_nt_col, cdr3_aa_col) %>%
#   group






# cdr3_nt_col <- "nSeqCDR3"
# cdr3_aa_col <- "aaSeqCDR3"
# col_names <- c(cdr3_nt_col, cdr3_aa_col)
# rep <- group1[[1]][col_names]
# colnames(rep) <- c("nt", "aa")



# fruits <- tibble(
#   type   = c("apple", "orange", "apple", "orange", "orange", "orange"),
#   year   = c(2010, 2010, 2012, 2010, 2011, 2012),
#   size  =  factor(
#     c("XS", "S",  "M", "S", "S", "M"),
#     levels = c("XS", "S", "M", "L")
#   ),
#   weights = rnorm(6, as.numeric(size) + 2)
# )
#
# # All possible combinations ---------------------------------------
# # Note that all defined, but not necessarily present, levels of the
# # factor variable `size` are retained.
# fruits %>% expand(type)




split_by_col <- function(df, cdr3_nt_col, cdr3_aa_col) {

  rep <- df[c(cdr3_nt_col, cdr3_aa_col)]
  colnames(rep) <- c("nt", "aa")
  aa_to_nt <- split(rep$nt, rep$aa) # split 2 vectors in a data frame
                                    # resulting a named list mapping
                                    # NT sequences groups to their
                                    # mutual encoding CDR3 AA sequence
  return(aa_to_nt)
}


# aa_to_nt <- split(rep$nt, rep$aa) # split 2 vectors in a data frame
                                  # resulting a named list mapping
                                  # NT sequences groups to their
                                  # mutual encoding CDR3 AA sequence


library("dplyr")

nt_col_name <- "nSeqCDR3"
aa_col_name <- "aaSeqCDR3"

# group1[[1]][aa_col_name]

rep_list <- lapply(group1, split_by_col, cdr3_nt_col="nSeqCDR3", cdr3_aa_col="aaSeqCDR3")
aa_list <- lapply(rep_list, names)

mapply(intersect, aa_list[1:2])



aa_intersect <- function(x,y) is.element(names(x), y)
aa_intersect <- function(pair) intersect(names(pair[[1]]), names(pair[[2]]))


pairs <- combn(rep_list, m = 2, simplify = FALSE, FUN = names)


?mapply(pairs, function(x) x)



# all_aa <- Reduce(union, lapply(rep_list, names))




aa_intersect(rep_list[1:2])



is.element(aa_list[[1]], aa_list[[2]])

aa_intersect()

combn(aa_list, m = 2, simplify = FALSE, FUN = function(x) list(pair=names(x), share=is.element(x[[1]], x[[2]])))






combn(aa_list, m = 2, simplify = FALSE, FUN = function(x) unlist(x, recursive = FALSE))

# intersect(rep_list[[1]][1:1000], rep_list[[2]][1:1000])


intersecing_aa <- intersect(names(rep_list[[1]]), names(rep_list[[2]]))


rep_list[[1]][intersecing_aa]

# unname(rep_list[[1]]["CAISDGGDTGELFF"])[[1]]==unname(rep_list[[2]]["CAISDGGDTGELFF"])[[1]]

library(tidyr)




?crossing(var1 = 0:1, var2 = 0:1, var3 = 0:1)

n <- 14
l <- rep(list(0:1), n)
expand.grid(l)






# intersect(rep_list$BRCA1, rep_list$BRCA2)
# intersect(unname(rep_list)[[1]], unname(rep_list)[[2]])

# all_aa <- unlist(lapply(rep_list, names), use.names = TRUE)

# which(table(all_aa) > 1, arr.ind = TRUE)

# length(unique(rep_list$BRCA1))


all_cdr3 <- Reduce(c, rep_list) # Function clusterMap and mcmapply (not Windows) in package parallel provide parallel versions of Map.





combn(unname(lapply(rep_list, names)), 2, simplify = FALSE, intersect)
lapply(rep_list, names)
lapply(rep_list, )



rep1 <- split_by_col(group1$BRCA1, "nSeqCDR3", "aaSeqCDR3")
rep2 <- split_by_col(group2$Control1, "nSeqCDR3", "aaSeqCDR3")


mapply(c, rep1, rep2, SIMPLIFY=FALSE)

?Reduce(union)

intersecting_aa <- intersect(names(rep1), names(rep2))

nt_vec <- unlist(rep$nt, use.names = FALSE)
aa_vec <- unlist(aa_col, use.names = FALSE)

unlist(group1[[1]][cdr3_nt_col], use.names = FALSE)
unlist(group1[[1]][cdr3_nt_col], use.names = FALSE)


nt_col <- rep[cdr3_nt_col]
aa_col <- rep[cdr3_aa_col]


split(nt_vec, aa_vec)  # split 2 vectors

user_input <- rep



# for 2 col input
if (is.data.frame(user_input) & ncol(nt_col)==1) {
  print("asd")
}


split_2_vec <- function(vec1, vec2) {
  split(nt_vec, aa_vec)
}


split_by_col <- function(df, col1, col2) {
  vec1 <- unlist(col1, use.names = FALSE)
  vec2 <- unlist(col2, use.names = FALSE)
  split_2_vec(vec1, vec2)
}

# split_by_col <- function(df, )

# mutual AA sequences
public_seq <- intersect(names(grouped_aa), names(x))
# list to df
unsplit(x, public_seq) %>% length









pair_list <- group1 %>% names %>% combn(2, simplify = FALSE)




