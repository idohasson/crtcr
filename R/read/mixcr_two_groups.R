
library("readr")
library("tools")
library("purrr")

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


files_from_dir <- function(dir_path, file_name_prefix) {
  paths <- list.files(dir_path, full.names=TRUE)
  paths_with_fields <- startsWith(basename(paths), file_name_prefix)
  return(paths[paths_with_fields])
}

list.files("data/Beta/", full.names=TRUE)

get_names <- function(paths) file_path_sans_ext(basename(p))

p <- files_from_dir("../../../dataset/clonotypes/Beta/", "BRCA")

df_list <- lapply(p, read_tsv, col_types = mixcr_column_type)




sample_name <- file_path_sans_ext(basename(p))
names(p) <- sample_name

l <- lapply(p, read_mixcr)

df <- flatten_dfr(l, .id = )


?list_to_df(l)
df$refPoints
read_mixcr(p[1])




dir_path = "../../../dataset/clonotypes/Beta/"
paths <- list.files(dir_path, full.names=TRUE)

group1_file_name_prefix <- "BRCA"
group2_file_name_prefix <- "Control"

group1_paths <- paths[startsWith(basename(paths), group1_file_name_prefix)]
group2_paths <- paths[startsWith(basename(paths), group2_file_name_prefix)]

names(group1_paths) <- file_path_sans_ext(basename(group1_paths))
names(group2_paths) <- file_path_sans_ext(basename(group2_paths))


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


group1 <- group1_paths %>% lapply(read_tsv, col_types = mixcr_column_type)
group2 <- group2_paths %>% lapply(read_tsv, col_types = mixcr_column_type)

rm(mixcr_column_type)
detach("package:dplyr", unload=TRUE)
detach("package:readr", unload=TRUE)
detach("package:tools", unload=TRUE)

