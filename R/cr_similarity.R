r <- ri(1, 2^16, 2^20) # sample(2^20, replace=TRUE, prob=c(.125,875))
all.as <- list(
  double = as.double
  , integer= as.integer
  , logical = as.logical
  , bit = as.bit
  , bitwhich = as.bitwhich
  , which = as.which
  , ri = function(x)x
)
all.types <- lapply(all.as, function(f)f(r))
sapply(all.types, object.size)
all.comb <- vector('list', length(all.types)^2)
all.id <- rep(NA, length(all.types)^2)
dim(all.comb)      <- dim(all.id)      <-    c(from=length(all.types),  to=length(all.types))
dimnames(all.comb) <- dimnames(all.id) <- list(from= names(all.types) , to= names(all.types))
for (i in seq_along(all.types))
  for (j in seq_along(all.as)){
    # coerce all types to all types (FROM -> TO)
    all.comb[[i,j]] <- all.as[[j]](all.types[[i]])
    # and test whether coercing back to the FROM type gives the orginal object
    all.id[i,j] <- identical(all.as[[i]](all.comb[[i,j]]),  all.types[[i]])
  }
all.id



fun1 <- function(x,y){
  x+y
}

fun2 <- function(x,y){
  x^y
}

fun3 <- function(x,y){
  x*y
}

models <- list(fun1 = fun1, fun2 = fun2, fun3 = fun3)



# models[["fun1"]](1,2)
# lapply(models, function(FUN, x, y){ FUN(x = 1, y = 2)})
ARGS <- purrr::cross2(cr1, cr2)
lapply(ARGS, do.call, what = fun1)

cr1 <- list(a=1,b=2,c=3); cr2 <- list(a=1,b=2,c=3)
rpois(1,1000)

# population 1 clones
c1 <- rand_rep_vec("aa", seq_n = rgeom(1,1E-3), seq_len = 3)
# population 2 clones
c2 <- rand_rep_vec("aa", seq_n = rgeom(1,1E-3), seq_len = 3)

# clonotype population count
cc1 <- vec_count(c1, sort = "location")
cc2 <- vec_count(c2, sort = "location")

# integer count of clonotypes
crlvl1 <- cc1$count
crlvl2 <- cc2$count


bit_unique(count_int)

# total share
tabulate(crlvl2)
# counting then combining
pmax(tabulate(crlvl1) ,tabulate(crlvl2))
# merging then counting
tabulate(merge_union(crlvl1, crlvl2))


# merge_notin(x,y)






