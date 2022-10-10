library("readr")
library("purrr")
library("dplyr")
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




dir_path <- "data/Beta/"

paths <- list.files(dir_path, full.names=TRUE)

paths <- paths[!grepl("metadata", paths)]

df <- paths %>%
  read_tsv(col_types = mixcr_column_type, id = "sample") %>%
  mutate(sample=file_path_sans_ext(basename(sample))) %>%
  add_count(aaSeqCDR3, wt = n_distinct(nSeqCDR3), sort = TRUE, name = "CRlevel") %>%
  add_count(aaSeqCDR3, wt = n_distinct(sample), name = "Sharing")

s <- df[1:10000,] %>%
  mutate(group=ifelse(startsWith(sample, "BRCA"), "Cancer", "Control")) %>%
  select(nSeqCDR3, group, sample) %>%
  table %>%
  addmargins

is_cancer <- s[,"Cancer","Sum"]>1
is_control <- s[,"Control","Sum"]>1


s[s[,"Sum","Sum"]>1,"Sum",]



nt_inclusive

s[is_cancer | is_control,,"Sum"]
# s[1,"Control",]


s[1:3,,"Sum"]==1
# %>%
#   filter(Sum==1)
#   ftable()

dcast()
with(head(df), table(nSeqCDR3, sample, CRlevel))

df %>%
  filter(Sharing>1) %>%
  with(table(aaSeqCDR3, sample))

df[,c("sample", "nSeqCDR3", "aaSeqCDR3", "Sharing", "CRlevel")] %>%
  head(100) %>%
  select(nSeqCDR3, sample) %>%
  table


distinct(df, nSeqCDR3, aaSeqCDR3, sample, sharing>1)

# add_count(aaSeqCDR3, wt = n_distinct(sample), sort = TRUE, name = "Sharing")
# select(df, sample, aaSeqCDR3, nSeqCDR3, CRlevel, Sharing) %>%
#   head



# df %>% select(sample, nSeqCDR3) %>%
#   group_by(sample) %>%
#   table(nSeqCDR3)



# df_list <- split(df, ~ sample)
# nt_list <- lapply(df_list, pull, var = "nSeqCDR3")
# nt_list <- lapply(df_list, pull, var = "nSeqCDR3", name = "aaSeqCDR3")





# df_list %>%
  # map(select, c())











