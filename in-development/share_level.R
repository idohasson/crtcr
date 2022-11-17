share_level <- function(...) {

  n_distinct(..., na.rm = TRUE)

}

ff <- function(.data, .clonotype, .rep_id, ..., share_func=share_level) {

  func <- function(.var)
    eval(as.call(list(func, quote(.var))))

  .data %>%

  group_by({{.clonotype}}, ..., .add = TRUE) %>%

  #   add_tally(wt = share_level({{.rep_id}}), name = "share")
  add_tally(wt = func({{.rep_id}}), name = "share")

}


df <- data.frame(
group_id=gl(2,4,8,labels = c('cancer','control')),
sample_id=gl(4,2,8,labels = LETTERS[1:4]),
nt=c('CGCGTGAAG', 'CGGGTGAAG','CACGAA','AAGGGGTCCGTG',
     'AAGGGGTCCGTC','CGGGTGAAG','AAGGGGTCCGTT','CGGGTGAAG'),
aa=c('RVK','RVK','HE','KGSV','KGSV','RVK','KGSV','RVK')
)

ff(df, aa, sample_id, group_id)

get_clonotype <- function(.data, attr_var, func)

f <- function(df, func, ...) {

  apply_func <- function(.var)
    eval(as.call(list(func, quote(.var))))



}
