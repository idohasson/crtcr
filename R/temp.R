library(vctrs)

share_number <- function(.clone, .rid, ...) {

  tapply(.rid, list(.clone, ...), vec_unique_count)

}

population_number <- function(.clone, .rid, .pid, ...) {

  tapply(.pid, list(.clone, .rid, ...), length)

}

time_series_number <- function(.clone, .rid, .pid, .tid, ...) {
  tapply(.tid, list(.clone, .rid, .tid, ...), length)
}



# is_public <- function(.tbl,
#                       public_func=NULL, public_condition=NULL,
#                       exclusive_func=NULL, exclusive_condition=NULL)





# (x <- t1["TCTCTACCA",,"1"])
# cr_func(x, func = sum, relative = T)



# crtcr::translate(.clone)
# cr_level

# remone NA
# crtcr::translate(.clone)
# stopifnot(vec_unique_count(.clonotype!=1))

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

cr_func <- function(.clone, .func=n_unique, relative=FALSE) {

  # cr_func

  lvl <- .func(.clone)

  # numerical_only <- vec_detect_complete(.clone)
  #
  # .clone <- .clone[numerical_only]

  if (isTRUE(relative))

    # lvl <- proportions(lvl)
    lvl <- lvl/vec_size(.clone)

  lvl
  # .func(.clone)

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
# split(c1, list(r1,g1))
# (r1, c1)
cr_level(c1)
cr_level(c1, r1)
cr_level(c1, r1, g1)
cr_level(c1, .cid=r1, g1, .cr_func = length)


cr_level(cc)
cr_level(cc, ct)
cr_level(cc, ct, rid)

cr_level(cc, cid=ct, rid=rid, pid=pid, time=gid) %>%

  apply(c("cid", "pid", "time"), is_public) %>% head %>%

  apply(c("cid", "pid"), diff) %>% apply(1:2, mean)




df_clone <- rand_df[rand_df$clonotype == "SLP",]
c1 <- sample(df_clone$clone, 32, rep=T)
r1 <- sort(sample(LETTERS[1:10], 32, rep=T))
# # r1 <- gl(4,1,32, LETTERS[1:4])
g1 <- gl(2,1,32,c("control", "cancer"))
w1 <- gl(4,1,32)


# (cr1 <- cr_number(c1, r1))
# (s1 <- share_number(c1, r1))
# (p1 <- population_number(c1, r1, g1))
# (t1 <- time_series_number(c1, r1, g1, w1))

x <- tapply(w1, list(c1, r1, g1), length)





