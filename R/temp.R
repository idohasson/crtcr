library(vctrs)
library(rlang)

cr_func <- function(..., .func=cr_level, .use_id=FALSE, .use_group=FALSE) {

  if (.use_id) {

    function(.id, ...) {

      .func(.id, ...)

    }

  } else if (.use_group) {

    function(.id, ...) {

      clone_list <- vec_chop(..., indices = .id$loc)

      vapply(clone_list, .func, numeric(1), ...)

    }

  } else {

    .func(...)

  }


}

apply_group <- function(x, ..., by_id=FALSE, .func) {

  dfl <- df_list(..., .name_repair = "unique_quiet")

  groups <- new_data_frame(dfl)

  if (by_id) {

    group_id <- vec_group_id(groups)

    .func(x, group_id)

  } else {

    group_loc <- vec_group_loc(groups)

    .func(x, group_loc)

  }

}


cr_mean <- function(.nt) {
  vec_unique_count(.nt) / attr(.nt, "n")
}

cr_number <- function(.nt) {
  vec_unique_count(.nt)
}

cr_level <- function(.clone, ..., .cr_func=cr_number) {
  vapply(.clone, .cr_func, numeric(1), ...)
}




nt_list <- vec_chop(.clone, indices = id$loc)

apply_group(z,x,.func = cr_level)

apply_group(z,x,.func = cr_level)

my_func <- cr_func(.func = cr_mean)

apply_group(z,x, by_id = TRUE, .func = my_func)




group_id <- apply_group(x, y)
cr_number(z)
cr_level(z, group_id)

# x <- sample(LETTERS[1:5], 30, rep=T)
# y <- sample(letters[1:3], 30, rep=T)
# z <- sample(replicate(20, rand_nt(3)), 30, rep=T)


grp_loc <- vec_group_loc(groups)
id <- vec_group_id(groups)

z

l <- vec_chop(z, groups$loc)

vapply(l, vec_unique_count, integer(1))




each_unique <- function(.data, .unique_by, ..., .each_func=n_unique) {

  indices <- vec_seq_along(.unique_by)

  unique_list <- vec_split(indices, by = .unique_by)

  .each_func(.data, unique_list$val)

}

unique_freq <- function(.freq_of, .unique_by) {

  freq_split <- vec_chop(.freq_of, .unique_by)

  compute_freq <- lapply(freq_of, vec_unique_count)

  list_unchop(compute_freq, indices = .unique_by)

}

unique_weight <- function(.weight, .unique_by) {

  id <- vec_group_id(.weight)

  id_list <- vec_chop(id, .unique_by)

  vapply(id_list, function(z) vec_unique_count(z) / attr(z,"n"), numeric(1))

}

vec_count(x, sort = "location")

x_l <- vapply(x_id$loc, length, numeric(1))

y_w <- vapply(vec_chop(y, x_id$loc), vec_unique_count, numeric(1))

x_l * y_w / length(x_id$key)




y <- sample(LETTERS[1:20], 100, rep=T)

x_id <- vec_group_loc(x)
y_id <- vec_group_loc(y)

x_id$key
y_id$key

x_id$loc
y_id$loc



each_unique(x, y, .each_func = unique_freq)

each_unique(rep(1, length(x)), x, .each_func = unique_weight)


freq <- unique_freq(x, ids$val)



tapply(x, y, vec_unique_count)

tapply(freq, y, mean)

list_unchop(as.list(freq), indices = ids$val)

tapply(freq, ids$key, mean)

tapply(x,y,vec_unique_count)

unique_freq(x,y)

vec_count(x, sort = "location")


# l <- vec_recycle_common(1, x)
# uid <- vec_group_id(x)
# uv <- each_unique(l[[1]], x)
# uv[uid] / attr(uid, "n")

weight_unique(x)
weight_unique(x, .weigh = a)
weight_unique(x, .weigh_unique = 2)
weight_unique(x, b, .weigh_func = mean)

weight_unique <- function(.data, .weigh=1, .weigh_unique=1, ..., .weigh_func=sum) {

  .weigh <- vec_recycle_common(.weigh, .data)[[1]]

  val_each_unique <- tapply(.weigh, .data, .weigh_func)

  # .weigh_unique <- vec_recycle_common(.weigh_unique, val_each_unique)[[1]]
  # val_each_unique <- val_each_unique * .weigh_unique / vec_size(val_each_unique)

  val_each_unique <- val_each_unique / vec_size(val_each_unique)

  val_each_unique[.data]
}

each_unique(x, y)

weight_unique(x)
vec_count(x)$count / 10
weight_unique(x, .weigh_func = sum)
weight_unique(x, .weigh_func = length)

x2 <- weight_unique(x)
each_unique(x2, y, .each_func = sd)
each_unique(rep_len(1,length(x)), y, .each_func = mean)




cr_number <- function(.clone, .id, ..., .cr_func=n_unique) {

  .cr_func(.clone, ...)

}

share_number <- function(.id, .share_func=n_unique, ...) {

  .share_func(.id, ...)

}

x <- sample(letters[1:10], 100, rep=T)
y <- sample(LETTERS[1:10], 100, rep=T)

a <- rpois(100, 2)
b <- runif(100)
c <- runif(100, min = -1)

cr_number(c("a", "b", "a"))
cr_number(c("a", "b", "a"), .cr_func = length)
cr_number(c(1,0,1))
cr_number(c(1,0,1), .cr_func = length)

share_number(c("a", "b", "a"))
share_number(c("a", "b", "a"), .share_func = length)
share_number(c(1,0,1))
share_number(c(1,0,1), .share_func = length)


each_unique(x,y, .each_func = cr_number)

each_unique(x,y)
each_unique(y,x)
each_unique(rep_along(x, 1), x)
each_unique(rep_along(x, 1), x)


cr_number(x, y)
share_number(x, y)

# in cr-level arg
# .each_unique=NULL
each_unique(x,y, cr_func)
# in share-level arg
# .each_unique=NULL
each_unique(y,x, share_func)

w=1;f=n_unique
each_unique(x,y,f)




v <- uv[x] / sum(uv)


each_unique(l[[1]],x , .each_func = length) # "mean"/z-score/

tapply(a, x, mean)
tapply(b, x, sum)
tapply(c, x, sd)



share_number <- function(.clone, .rid, ...) {

  tapply(.rid, list(.clone, ...), vec_unique_count)

}

share_number <- function(.clone, .rid, ...) {

  tapply(.rid, list(.clone, ...), vec_unique_count)

}

population_number <- function(.clone, .rid, .pid, ...) {

  tapply(.pid, list(.clone, .rid, ...), length)

}

time_series_number <- function(.clone, .rid, .pid, .tid, ...) {

  tapply(.tid, list(.clone, .rid, .tid, ...), length)

  # library(magrittr)
  # cr_level(cc, cid=ct, rid=rid, pid=pid, time=gid) %>%
  #
  #   apply(c("cid", "pid", "time"), is_public) %>%
  #
  #   apply(c("cid", "pid"), diff) %>% apply(1:2, mean)

}




is_public <- function(v, ..., min_count=2,
                      public_func=NULL, public_condition=NULL) {

  v_val <- if (is.null(public_func)) n_unique(v) else public_func(v)

  is_p <- if (is.null(public_condition)) v_val >= min_count else public_condition(v_val)

  is_p

}

is_exclusive <- function(v, ..., max_count=1,
                      exclusive_func=NULL, exclusive_condition=NULL) {

  v_val <- if (is.null(exclusive_func)) n_unique(v) else exclusive_func(v)

  is_e <- if (is.null(exclusive_condition)) v_val <= max_count else exclusive_condition(v_val)

  is_e

}


x <- c(1, 0, 1)

is_public(x)
is_public(x, min_count = 3)

is_public(x, public_func = length)
is_public(x, min_count = 4, public_func = length)

my_condition <- function(z) z / length(x) > .5
is_public(x, public_condition = my_condition)
is_public(x, public_func = length, public_condition = my_condition)


is_exclusive(x)

is_exclusive(x, max_count = 2)

is_exclusive(x, exclusive_func = length)
is_exclusive(x, max_count = 2, exclusive_func = length)

is_exclusive(x, exclusive_condition = my_condition)
is_exclusive(x, exclusive_func = sd, exclusive_condition = my_condition)



n_unique_df <- function(df, na.rm=FALSE) {

  if (isTRUE(na.rm)) {

    no_na <- vec_detect_complete(df)

    dfi <- vec_slice(dfi, no_na)

  }

  vec_unique_count(df)

}

n_unique_vec <- function(...) {

  if (rlang::dots_n(...) == 1) {

    v <- c(...)

    if (is.numeric(v))

      return(n_unique_num(v))

    else

      return(vec_unique_count(v))

  }

  dfl <- df_list(..., .name_repair = "minimal")

  dfi <- new_data_frame(dfl)

  n_unique_df(dfi)

}

n_unique_list <- function(..., each_flat=FALSE, flat_num=FALSE) {

  l <- rlang::dots_splice(...)

  if (isTRUE(each_flat)) {

    l <- lapply(l, unlist, use.names = FALSE)

    n <- vapply(l, n_unique_vec, integer(1L))

    if (isTRUE(flat_num))

      return(n_unique_num(n))

    else

      return(n)

  } else {

    l <- unlist(l, use.names = FALSE)

    return(n_unique_vec(l))

  }





}

n_unique_lgc <- function(logical_vec, na.rm = FALSE) {

  if (isTRUE(na.rm))

    logical_vec <- logical_vec & vec_detect_complete(logical_vec)

  sum(logical_vec)

}

n_unique_num <- function(num_vec, na.rm = FALSE) {

  lgc <- as.logical(num_vec)

  n_unique_lgc(lgc, na.rm)

}

n_unique_arr <- function(num_array, margins=1, na.rm = FALSE) {

  apply(num_array, margins, n_unique_num, na.rm)

}

n_unique <- function(x, ...) {

  if (is.numeric(x))

    if (is_dim(x))

      return(n_unique_arr(x, ...))

    else

      return(n_unique_num(x, ...))

  if(is.vector(x))

    if (is.list(x))

      return(n_unique_list(x, ...))

    else

      return(n_unique_vec(x, ...))

  else if (is.data.frame(x, ...))

    return(n_unique_df(x, ...))

  else

    stop("Unsupported input type")

}

is_dim <- function(x) {

  d <- dim(x)

  if (is.null(d))
    return(FALSE)

  else if (length(d)==1)
    return(FALSE)

  else
    return(TRUE)

}






cr_number <- function(.clone, .relative_cr=FALSE) {

  cr_n <- n_unique(.clone)

  if (isTRUE(.relative_cr)) cr_n <- cr_n / vec_size(.clone)

  cr_n

}

cr_func <- function(.clone, .func=cr_number, ...) {

  lvl <- .func(.clone)

  if (isTRUE(relative))

    lvl <- lvl/vec_size(.clone)

  lvl

}

cr_level <- function(.clone, ..., .cr_func=n_unique) {

  if (rlang::dots_n(...)==0)

    return(.cr_func(.clone))


  .id <- rlang::dots_splice(...)

  tapply(.clone, .id, .cr_func)

}

cc <- rand_df$clone
ct <- rand_df$clonotype
rid <- rand_df$rep_id
pid <- sample(c("cancer", "control"), nrow(rand_df), replace = T)
gid <- sample(1:4, nrow(rand_df), replace = T)

cr_level(cc)
cr_level(cc, ct)
cr_level(cc, ct, rid)



power2 <- function(exp) {
  force(exp)
  function(x) {
    x == exp
  }
}

x <- 2
square <- power2(x)
x <- 3
square(2)


new_counter <- function(i, condition) {
  # i <- 0
  force(i)
  function(j) {
    # i <<- i + j
    # i
    i > j
    # alist(a = , b = i, condition)
  }
}

my_counter2 <- new_counter(2, i * j)
my_counter2(1)
my_counter2(2)
my_counter2(3)
my_counter2(10)


foo <- function(public_min, public_condition) {

  force()

  l <- alist(a = , b = public_min, public_condition)

  as.function(l)

  # function(v) f()

}

my_foo1 <- foo(public_min = 2, public_condition = a > b)
my_foo1(1)

foo <- function(public_min, public_condition) {

  function(p) p**public_min
  # force(public_min)
  # l <- alist(a = , b = public_min, public_condition)
  # as.function(alist(a = , b = public_min, public_condition))
}

foo <- as.function(alist(a = , public_min = 2, a>public_min))
foo(3)


# as.function(alist(a = , public_min = 2, a>public_min))

make_foo <- function(my_min, my_condition) {
  # as.function(alist(a = , public_min = my_min, a>public_min))
  as.function(alist(v = , public_min = my_min, my_condition))
}
my_foo <- make_foo(my_min = 3, v>public_min)
my_foo <- make_foo(my_min = 3, v>my_min)
my_foo(4,public_min = 1)





f <- function(l) as.function(l)
x <- alist(a = , b = 2, a > b)
foo <- f(x)


foo(3)


# f <- as.function(alist(a = , b = 2, a > b))
my_f <- f(x)
my_f(3)
x <- alist(a = , b = 2, sum(as.logical(a)) > b)
my_f2 <- f(x)
my_f2(c(1,2))

v <- vector(length = 5)
for(i in 1:5) v[i] <- my_f2(seq(i))
v[1]

















