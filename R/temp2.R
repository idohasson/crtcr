
grp1 <- rand_nt_vec()
grp2 <- rand_nt_vec()


library(magrittr)

# cr level

cr_seq1 <- translate(grp1)
cr_seq2 <- translate(grp2)

(cr1 <- vec_group_loc(cr_seq1))
(cr2 <- vec_group_loc(cr_seq2))

#         get cr sequences            compute cr number
lvl1 <- vec_chop(grp1, cr1$loc) %>% sapply(vec_unique_count)
lvl2 <- vec_chop(grp2, cr2$loc) %>% sapply(vec_unique_count)

# pairwise
# compare 2 cr level        find matching aa
lvl1 / vec_slice(lvl2, vec_match(cr1$key, cr2$key))



# share level
rep_id1 <- sample(LETTERS[1:3], length(grp1), rep=T)
rep_id2 <- sample(LETTERS[4:6], length(grp1), rep=T)

# share number

(share1 <- vec_chop(rep_id1, cr1$loc) %>% sapply(vec_unique_count))
(share2 <- vec_chop(rep_id2, cr2$loc) %>% sapply(vec_unique_count))


# cr_number
clones <- vec_c(grp1, grp2)
rep_id <- vec_c(rep_id1, rep_id2)
group <- rep(c("cancer", "control"), each=vec_size(grp1))

cr <- vec_group_loc(translate(grp1))

cr$loc

id <- vec_cbind(rep_id, clones) %>% vec_chop(cr1$loc) %>% lapply(vec_count)



cr_number <- function(cr_id, freq) mean(cr_id$count * freq / sum(freq))
mapply(cr_number, id, vec_chop(runif(length(clones)), cr1$loc))
lapply(id, )

x <- vec_split(cc[[1]], id[[1]])
y <- vec_split(id[[1]], cc[[1]])


lapply(y$val, function(i) vec_count(i)$count)


# cr_level <- function()












