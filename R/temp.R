library(vctrs)
library(rlang)

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

















