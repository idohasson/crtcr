# Convergent recombination level calculation.
# unique number of values / rows.

# nt_vec <- c("ATGTTT", "ATGTTC", "ATGTTT", "ATGTTA",
            # "ATGTTT", "ATGTTC", "ATGTTT", "ATGTTG")



# cr_level(nt_vec)
#
# # For clones from multiple repertoires, each of the clonotype's (here, the
# # corresponding amino acid sequences) CR-level value should be calculated
# # separately.
#
# sample_id <- gl(3,2, labels = paste0("sample_", seq(3)))
# sample_id # same length vector, indicating which sequence belong to which sample
#           # by a unique id (character / numeric / logical / etc.)
#
# cr_level(nt_vec, sample_id) # Incorrect way - this is the sum of all CR-level.
# # Alternative way
# samples_split <- split(nt_vec, sample_id)
# samples_split # list of clonal sequence vectors, each belongs to different sample.
# # Repertoires' CR-level of a certain clonotype.
# sapply(samples_split, cr_level)






cr_level <- function(.clonal_seq, ..., cr_func=NULL) {

  clone_data <- vctrs::df_list(.clonal_seq, ..., .name_repair = "minimal")

  clone_data <- vctrs::new_data_frame(clone_data)

  vctrs::vec_unique_count(clone_data)

  # if (isTRUE(na.rm)) {
  #
  #   no_na <- vctrs::vec_detect_complete(clone_data)
  #
  #   clone_data <- vctrs::vec_slice(clone_data, no_na)
  #
  # }

  # if (is.null(cr_func)) {
  #
  #   results <- vctrs::vec_unique_count(clone_data)
  #
  # } else {
  #
  #   results <- cr_func(clone_data)
  #
  # }
  #
  # results



}


cr_freq <- function(.clone, .freq, across_freq=sum, across_clone=mean) {

  across_clone(tapply(.freq, .clone, across_freq))

}




