# Split the vector into subgroups
groups <- split(vec, subgroups)

# Count the number of overlapping values in each subgroup
overlap <- sapply(groups, function(x) sum(x %in% vec))

aa_i=c("a", "e"); group_i=c("C", "D")
aa_id <- function(i) vec_match(i, letters[1:5])
group_id <- function(i) vec_match(i, LETTERS[1:5])
aa_id(aa_i);group_id(group_i)

library(vctrs)
aa1=sample(letters[1:5],20,T)
aa2=sample(letters[1:5],20,T)
id1=sample(LETTERS[1:5],20,T)
id2=sample(LETTERS[1:5],20,T)


i=vec_group_loc(id)
subgroups <- vec_chop(aa, i$loc)


set1=split(aa1, id1);set2=split(aa2, id2)


# If x is a subset of y, returns -1
# If y is a subset of x, returns 1
# If sets are equal returns 0.
set1$A
set2$A
vec_c(set1)
vec_c(set2)
mapply(subsetOrder, set1, set2)
