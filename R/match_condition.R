# is_super_public <-  function(cr_count) {
#   super_min_freq <- ~ top_prec(.x, top = .5)
#   level_values <- level(cr_count, super_min_freq)
#   condition(cr=level_values, .condition = cr != 0)
# } 
# is_super_public(x)
# condition(x, y, .condition = x>=2 & y<4)
condition <- function(..., .condition) {
  
  condition_call <- substitute(.condition)
  
  dfl <- df_list(..., .name_repair = "minimal")
  
  val <- new_data_frame(dfl)
  
  i_cond <- eval(condition_call, val)
  
  i_cond
  
}


# share_level <- function(aa, id) {
#   vapply(vec_split(id, aa)$val, vec_unique_count, numeric(1))
# }
# 
# public_level <- function(aa, id1, id2, 
#                          share_val1=share_level(aa, id1), 
#                          share_val2=share_level(aa, id2), 
#                          bound=list(2,1), condition=c(">=", "<=")) {
#   
#   match_condition(public=share_val1,
#                   exclusive=share_val2,
#                   bound_level = bound, 
#                   condition = condition)
#   
# }
# AA=translate(rand_nt_vec(1000, 2))
# ID1=rep_along(AA, letters[1:16])
# ID2=rep_along(AA, LETTERS[1:3])
# unique(AA[public_level(AA, ID1, ID2)==1])
match_condition <- function(..., bound_level=1, 
                            condition=c(">", ">=", "==", "<=", "<")) {
  
  dfl <- df_list(..., .name_repair = "unique_quiet")
  
  value_df <- new_data_frame(dfl)
  
  # Check valid bound
  if (!is.data.frame(bound_level)) {
    
    bound_list <- splice(bound_level)
    
    var_names <- names(value_df)
    
    bound_list <- rep_named(var_names, vec_recycle_common(bound_list))
    
    bound_level <- list2DF(bound_list)
    
  }
  
  condition <- rep_along(value_df, condition)
  
  # condition
  matches <- vec_locate_matches(

    needles = value_df, haystack = bound_level, condition = condition,

    multiple = "last", incomplete = NA,

    needles_arg = "*_values", haystack_arg = "*_bound"

  )

  matches$haystack
  
}

