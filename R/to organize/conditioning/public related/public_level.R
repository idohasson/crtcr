public_level <- function(aa, id1, id2,
                         share_val1=share_level(aa, id1),
                         share_val2=share_level(aa, id2),
                         bound=list(2,1), condition=c(">=", "<=")) {

  match_condition(public=share_val1,
                  exclusive=share_val2,
                  bound_level = bound,
                  condition = condition)

}
