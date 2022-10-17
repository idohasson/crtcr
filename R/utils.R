library("tidyselect")
library("dplyr")

library("tibble")
library("purrr")
library("forcats")

library(readr)

# paths <- list.files("../../../Datasets/Multiple sampled mice/data/Beta/", full.names = TRUE)[-7]
dfl <- lapply(paths, read_tsv, show_col_types = FALSE)

list.df <- read_delim(paths,
                      delim = "\t", escape_double = FALSE,
                      trim_ws = TRUE)


get_clones.df <- function(df, ...) {

  vars <- c(...)

  as_tibble(df) %>%
    distinct_at(vars)
  # get_clones.df(df, "nSeqCDR3", "aaSeqCDR3")
}

get_gruops <- function(dfl, gl, ...) {

  if (is.null(names(dfl))) {
    names(dfl) <- LETTERS[seq_along(dfl)]
  }

  if (is.null(names(gl))) {
    names(gl) <- letters[seq_along(gl)]
  }


  get_df <- function(indices) map_dfr(dfl[indices], get_clones.df, ..., .id = "sample")

  map_dfr(gl, get_df, .id = "group")

  # get_gruops(dfl, list(1:2, 3:4, 5:6))
}



# get_df <- function(dfl, g1, g2, ...) {
#   g <-  list(g1, g2, ...)
#   map(dfl, fanction())
#
# }

# a list of character data-frames
# get_clones.df.list <- function(dfl, nt, ...) {
#
#   vars <- c(nt, ...)
#   map_dfr(dfl, get_clones.df, vars, .id = "sample")
#
#   # list.df %>%  get_clones.df.list("nSeqCDR3", "aaSeqCDR3")
# }


is_singltone <- function(v) {
  sum(as.logical(v))==1
}

is_singltone.table <- function(tbl, in_cols) {

  if (!missing(in_cols)) {
    tbl <- tbl[,in_cols]
  }

  apply(tbl, 1, is_singltone)
}

clonotype_table <- function(clones) {
  cr_lvl.list(clones) %>%
    with(table(f, sample))
}

rand_aa <- function(vec_size, l=5) {
  library("stringi")
  do.call(paste0, Map(stri_rand_strings, n=vec_size, length=l, pattern = '[A-C]'))
}



n <- 100
g <- 4

df <- data.frame(seq=rand_aa(n),
                 sample=gl(g, 1, n, labels = letters[1:g]))
#
# col1 <- "seq"
# col2 <- "sample"
#
# split(df[,col1], pull(df, col2))
#
# map(split(df, ~ col1), pull, col1)
#
#
# pull(df, seq, sample)
#
# rep_list <- replicate(12, rand_aa(10, 2), simplify = FALSE)
#
# cr_lvl(df$seq)


fl <- map(seq_list, factor)

public <- fct_c(!!!map(fl, fct_unique)) %>%
  fct_lump_min(2, "private")


# fct_unify(clonotype_list, levels = levels(public_clonotype))
# group_clonotype <- map(list(1:4, 5:8, 9:12), function(i) levels(fct_c(!!!aa_list[i])))
# map(sample_groups, fct_lump_min, min = 2, other_level = "exclusive")
# sample_groups <- fct_c(!!!sample_groups)

# e <- fct_lump_min(sample_groups, min = 2, other_level = "exclusive")
# fct_other(public, keep = "private", other_level = "public")


# stats <- c("private", "public")
# as.factor()
# cbind(is_singltone.table(x))



