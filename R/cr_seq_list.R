# TODO list of each sub population

# public_seq.df <- function(df) {
#
#   is_singltone <- function(vec) sum(as.logical(vec))==1
#
#   t <- df %>%
#     with(table(sample, group, aaSeqCDR3)) %>%
#     as.data.frame %>%
#     xtabs(formula = Freq ~ aaSeqCDR3+group) %>%
#     addmargins(margin = 2,
#                FUN = list(list(sample_freq=sum,
#                                gruop_freq=is_singltone)))
#
#   pri_seq <- t[t[,"sample_freq"]==1,] %>% rownames
#   # pub_seq <- t[t[,"sample_freq"]!=1,] %>% rownames
#   ex_seq <- t[t[,"sample_freq"]!=1 & t[,"gruop_freq"]==1,] %>% rownames
#   in_seq <- t[t[,"sample_freq"]!=1 & !t[,"gruop_freq"]==1,] %>% rownames
#
#   list(private=pri_seq, public=list(exclusive=ex_seq, inclusive=in_seq))
# }
