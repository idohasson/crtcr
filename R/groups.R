# Split the vector into subgroups
groups <- split(vec, subgroups)

# Count the number of overlapping values in each subgroup
overlap <- sapply(groups, function(x) sum(x %in% vec))

library(vctrs)
x=sample(letters[1:5],20,T)
y=sample(LETTERS[1:5],20,T)


aa_i=c("a", "e"); group_i=c("C", "D")

aa_id <- function(i) vec_match(i, letters[1:5])
aa_id(aa_i)
group_id <- function(i) vec_match(i, LETTERS[1:5])
group_id(group_i)


aa_id(x)
group_id(y)

vec_group_loc(seq_along(x))$loc
group_count <- function(group, x=seq_along(group)) {
  vec_split(y, x)
}
group_count
