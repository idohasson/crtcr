#
#
# # TODO: unique clonotype vector
# # TODO: add from_file option
#
#
# # extract the nucleotide sequences from the data frame.
# if (missing(nt)) nt <- search_field(rep, "nt")
# stopifnot(is.character(nt))
# # extract the amino acid sequences from the data frame.
# if (missing(aa)) nt <- search_field(rep, "aa")
#
#
# # if (is.character(clonotype_col)) distinct_at(rep, c(clonotype_col, clone_col), .keep_all = TRUE)
#
# # rep %>% pull(clone_col) %>% unique() %>% translate()
#
# # cbind.data.frame(clone = clone_col, clonotype = translate(clone_col))
# # else:
# #   cbind.data.frame(clone = clone_col, clonotype = translate(clone_col))
#
# # if (is.null(aa) & ! is.null(nt)) aa <- translate(nt)
#
#
#
#
# # aa <- get_aa(rep, "aa")
# # convert the nucleotide sequence to an amino acid sequence.
#
# # cbind.data.frame(clone = nt, clonotype = aa) %>%
#
#   # distinct_at(clone, .keep_all = TRUE)
# #
#
#
#
#
# # extract data by clone definition
# # 1. unique pairing of clonotype and clonal sequence
#
#
#
# #
# # get_clonotypes <- function(..., clone, clonotype) {
# #
# #   if (is.data.frame(...)) {
# #
# #     df <- data.frame(...)
# #
# #     distinct_at(df, c(clone, clonotype))
# #
# #     return(df)
# #
# #   }
# #
# #   l <- list2(...)
# #
# #   if (length(l) == 2 & lapply(l, is.character)) {
# #
# #     v1 <- l[[1]]; v2 <- l[[2]]
# #
# #     v <- v1[!(duplicated(v1) & duplicated(v2))]
# #
# #     return(v)
# #
# #   }
# #
# # }
# #
# # # vector
# #
# # get_clonotype_v <- function(v1, v2, return_both = FALSE) {
# #
# #   d <- !(duplicated(v1) & duplicated(v2))
# #
# #   if (!return_both) {
# #
# #     return(v1[d])
# #
# #   }
# #
# #   list(v1[2], v2[d])
# # }
# #
# # # data frame
# # get_clonotypes_df <- function(df, clone, clonotype) {
# #
# #   if (missing(clonotype)) clonotype_col <- search_field(rep_data, "aa")
# #
# #   if (missing(clone)) clone_col <- search_field(rep_data, "nt")
# #
# # }
#
#
#
#
# # get_group_clonotype
#
# # get_multiple_group_clonotype
#
#
#
# get_clonotypes <- function(rep, clone_col, clonotype_col=NULL, out_type="data.frame") {
#
#   # TODO: unique clonotype vector
#   # TODO: add from_file option
#   if (!hasName(clone_col)) clone_col <- search_field("nt")
#
#   stopifnot(clone_col)
#
#
#   # out_type <- arg_match(out_type, c("list", "data.frame", "vector", "unique"))
#
#   # if (out_type == "list") {
#     # search_field(rep ,"aa")
#     # if (missing(clonotype_col)) clonotype_col <- search_field(df ,"aa")
#     # if (hasName(df, clonotype_col)) clonotype_col <- search_field(df ,"aa")
#     # if (hasName(df, clone_col)) clone_col <- search_field(df ,"nt")
#     # is.null(clonotype_col) %>% return()
#     # print(is.na(clonotype_col))
#
#     # if ()
#     # if (is.null(aa) & ! is.null(nt)) aa <- translate(nt)
#     # extract the nucleotide sequences from the data frame.
#     # nt <- get_nt(df, clone_col)
#     # extract the amino acid sequences from the data frame.
#     # aa <- get_aa(df, clonotype_col)
#     # if (is.null(aa) & ! is.null(nt)) aa <- translate(nt)
#     # distinct_at(df, c(clone_col, clonotype_col))
#     # # splits a vector into a list of vectors.
#     # clonotypes <- split(nt, aa) %>%
#     #   # return a list of unique clonal sequences (NT) named by their
#     #   lapply(unique) # corresponding clonotype sequence(AA).
#
#   # } else if (out_type == "data.frame") {
#   #   # extract the nucleotide sequences from the data frame.
#   #   nt <- get_nt(df, clone_col)
#   #   # extract the amino acid sequences from the data frame.
#   #   aa <- get_aa(df, clonotype_col)
#   #   # convert the nucleotide sequence to an amino acid sequence.
#   #   if (is.null(aa) & ! is.null(nt)) aa <- translate(nt)
#   #   # binds columns together into a data frame.
#   #   clonotypes <- cbind.data.frame(clone = nt, clonotype = aa) %>%
#   #     # returns the unique rows of a data frame.
#   #     distinct(clone, .keep_all = TRUE)
#   #
#   # } else if (out_type == "vector") { # vector
#     # binds columns together into a data frame.
#   #   clonotypes <- cbind.data.frame(nt, aa) %>%
#   #     # returns clonotype sequences (AA) by identifying unique pairing to their
#   #     vec_unique_loc %>% df[.,] # corresponding clonal sequence (NT).
#   # }
#
#   # clonotypes
#
# }
#
# get_group_clonotype <- function(df_list, ..., to_df=FALSE, each_unique=FALSE) {
#   # check that df_list is a list.
#   stopifnot(is.list(df_list))
#   # check that every element of df_list is a data frame.
#   stopifnot(purrr::every(df_list, is.data.frame))
#   # applies the get_clonotypes function to each element of df_list.
#   if(!each_unique) {
#
#     df_list <- lapply(df_list, get_clonotypes, ...)
#
#     if (!to_df) return(df_list)
#
#     if (!is_named(df_list)) names(df_list) <- as.character(seq_along(df_list))
#
#     df <- bind_rows(df_list, .id = "sample")
#
#     return(df)
#
#   }
#   # It checks if the ellipsis_args dictionary has a key named f_arg.
#   ellipsis_args <- list2(...); f_arg <- formalArgs(get_aa)[2]
#   # If it does, it returns the value of that key. If it doesn't, it returns an empty list.
#   arg_l <- if (has_name(ellipsis_args, f_arg)) ellipsis_args[f_arg] else list()
#   # takes a list of data frames and returns a list of unique amino acid sequences.
#   lapply(df_list, function(x) unique(do.call("get_aa", append(list(df=x), arg_l))))
# }
#
# get_multiple_group_clonotype <- function(df_list, ..., group_to_df=FALSE, indices) {
#
#   stopifnot(is.list(df_list))
#
#   # if (!missing(indices))
#   #   df_list <- list(lapply(indices, function(i) df_list[i]))
#
#   stopifnot(purrr::every(unlist(df_list, recursive = FALSE), is.data.frame))
#
#   df_list <- lapply(df_list, get_group_clonotype, ...)
#
#   if (!group_to_df) return(df_list)
#
#   if (!is_named(df_list)) names(df_list) <- as.character(seq_along(df_list))
#
#   bind_rows(df_list, .id = "gruop")
#
# }
#
# get_clonotypes(df, clonotype_col = "aa", clone_col = "nt", "list")
#
#
# get_clonotypes_list <- function(df_list, to_df=FALSE, ..., indices) {
#   # check that df_list is a list.
#   stopifnot(is.list(df_list))
#   # check that every element of df_list is a data frame.
#   stopifnot(purrr::every(df_list, is.data.frame))
#   # checks if the list is named, and if so, it sets the names of the data frames
#   # to the sequence of integers starting from 1.
#
#
#
#
#
#   if (to_df) {
#
#     if (!is_named(df_list)) names(df_list) <- as.character(seq_along(df_list))
#
#     df <- df_list %>%
#       # Get the list of clonotypes data frames for each repertoire.
#       get_clonotypes_list(out_type = "data.frame", ...) %>%
#       # Bind the list of clonotypes for each sample into a single data frame.
#       bind_rows(.id = "sample")
#
#     return(df)
#   }
#
#   lapply(df_list, get_clonotypes, ...)
#
# }
#
#
