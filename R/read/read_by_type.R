library("purrr")
library("readr")
library("tools")


mixcr_column_type <- cols(
  .default = col_logical(),
  cloneId = col_double(),
  cloneCount = col_double(),
  cloneFraction = col_double(),
  targetSequences = col_character(),
  targetQualities = col_character(),
  allVHitsWithScore = col_character(),
  allDHitsWithScore = col_character(),
  allJHitsWithScore = col_character(),
  allCHitsWithScore = col_character(),
  allVAlignments = col_character(),
  allDAlignments = col_character(),
  allJAlignments = col_character(),
  allCAlignments = col_character(),
  nSeqCDR3 = col_character(),
  minQualCDR3 = col_double(),
  aaSeqCDR3 = col_character(),
  refPoints = col_character()
)

get_names <- function(use_base_name) file_path_sans_ext(basename(use_base_name))


read_mixcr <- function(path) {

  read_tsv(path, col_types = mixcr_column_type)
}

read_mixcr_dir <- function(paths) {
  lapply(paths, read_tsv, col_types = mixcr_column_type, id = "sample") %>%
    map_dfr(as.data.frame, .id = "sample")
}


# read_paths <- list.files("data/Alpha/", full.names = TRUE)[-7]
# df <- read_mixcr_dir(read_paths)
