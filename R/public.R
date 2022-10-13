library("readr")
library("purrr")
library("dplyr")
library("tools")





sample_paths <- list.files("../../../dataset/clonotypes/Beta/", full.names=TRUE)
data_list <- sapply(sample_paths, read_mixcr, simplify = FALSE)
names(data_list) <- names(data_list) %>% basename %>% file_path_sans_ext
aa_colname <- "aaSeqCDR3"

group1 <- which(startsWith(names(data_list), "BRCA"))
group2 <- which(!startsWith(names(data_list), "BRCA"))

groups <- list(cancer=names(data_list)[group1], control=names(data_list)[group2])

groups

aa_list <- data_list %>% lapply(pull, aa_colname)
aa_unique <- aa_list %>% lapply(unique)

aa_count <- table(unlist(aa_unique, use.names = FALSE))

public_aa <- aa_count[aa_count>1]
private_aa <- aa_count[aa_count==1]

cancer_aa <- Reduce(union, aa_unique[groups$cancer])
control_aa <- Reduce(union, aa_unique[groups$control])


private_aa <- names(private_aa)
inclusove_aa <- intersect(cancer_aa, control_aa)
exclusove_aa <- union(setdiff(names(public_aa), control_aa), setdiff(names(public_aa), cancer_aa))




length(exclusove_aa)


x <- df %>%
  sample_n(10) %>%
  distinct(nSeqCDR3, aaSeqCDR3, sample, group) %>%
  with(table(aaSeqCDR3, sample, group)) %>%
  ftable(row.vars = "aaSeqCDR3", col.vars="group")

# x[,.(public=sum(as.logical())), .G="Cancer"]
x[aa,]
# x[.(sum(as.logical(aaSeqCDR3))==1),C:="public"]


clone_table["Sum",,] %>%
  add_count()

?ftable
clone_table[,,"Sum"]
clone_table


as.data.table(df) %>%
  .[,n_distinct(sample), by=c("group", "aaSeqCDR3")]

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











