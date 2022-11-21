#' Convergent recombination level calculation.
#'
#' unique number of values / rows.
#'
#' @param ...
#'
#' @return integer
#' @export
#'
#' @examples
#'
#' cr_level(c("A", "B","A", "B", "B", NA))
#'
#' cr_level(c("s", "s", "a", "a"), c(1:3,NA), na.rm = FALSE)
#'
cr_level <- function(..., func) {

  clone_data <- vctrs::df_list(..., .name_repair = "minimal")

  clone_data <- vctrs::new_data_frame(clone_data)

  vctrs::vec_unique_count(clone_data)
}

# library(magrittr)
# library(purrr)

library(vctrs)
library(dplyr)

dir_path <- getwd() %>% paste("/in-development/beta_MiXCR/", sep = "")
paths <- list.files(dir_path, full.names = TRUE)

dfl <- lapply(paths[1:5], read.delim, sep = "\t", header = TRUE)
dfl2 <- lapply(dfl, sample_n, size = sample(500:1500,1), replace = T)
df <- bind_rows(dfl2, .id = "r_id")


f <- function(..., cr_func=NULL, args=NULL) {

  clone_data <- vctrs::df_list(..., .name_repair = "minimal")

  clone_data <- vctrs::new_data_frame(clone_data)

  if (is.null(cr_func)) {
    return(vctrs::vec_unique_count(clone_data))
  } else {

    unique_ids <- vec_duplicate_id(clone_data)

    if ()

    # return(clone_data)
    # return(my_func(unique_ids))
  }

}

x <- data.frame(x=c("A", "A", "A", "B"), y=c("C", "C", "D", "D"))

args1 <- list(1:4, 8:5)
args12 <- list2DF(args1)

tapply(vec_duplicate_id(x))

lapply(i$loc, function(i), args12)

my_func <- function(unique_ids) mean(table(unique_ids))

# df %>%
#   group_by(aaSeqCDR3) %>%
#   summarise(cr=f(nSeqCDR3, r_id, cr_func = my_func))

df[df$aaSeqCDR3=="CAGDWGNYAEQFF",] %>%
  summarise(cr=f(nSeqCDR3, r_id, cr_func = my_func, args = list(cloneFraction)))
# df[df$aaSeqCDR3=="CASSFRDNQDTQYF",] %>%
  f(cr_func = my_func)
  # f()


x1 <- df %>% group_by(aaSeqCDR3, r_id) %>%
  summarise(cr=f(nSeqCDR3))
x1[vec_duplicate_detect(x1$aaSeqCDR3),]


df %>% group_by(aaSeqCDR3) %>%
  summarise(cr=f(nSeqCDR3, r_id, cr_func = mean))

converged_from=c("nSeqCDR3", "allVHitsWithScore"); converged_to=c("aaSeqCDR3", "allJHitsWithScore")

converged_from=c("nSeqCDR3"); converged_to=c("aaSeqCDR3")
df2 <- dplyr::sample_n(df, 200, replace = TRUE)
# nrow(df2)
# df2 <- rbind(df,df)
x <- vec_split(df2[clonal_seq], df2[clonotype])




sapply(x$val, vec_duplicate_id)

x$key %>% head

# vec_duplicate_id()
# sapply(x$val, vec_unique_count)
