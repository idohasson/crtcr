library("readr")
library("purrr")
library("dplyr")
library("tools")





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











