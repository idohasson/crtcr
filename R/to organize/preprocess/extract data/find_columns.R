# get_pos(rand_rep_df(), nt, aa)
get_pos <- function(.data, .clone, .clonotype, .repertoire, ...) {

  # TODO: check cols

  clone_pos <- eval_select(enquo(.clone), .data)

  clonotype_pos <- eval_select(enquo(.clonotype), .data)

  repertoire_pos <- eval_select(enquo(.repertoire), .data)

  group_pos <- eval_select(expr(c(...)), .data)

  list(clone=clone_pos,
       clonotype=clonotype_pos, # TODO VDJ
       repertoire=repertoire_pos,
       group=group_pos) # TODO more than one group

  # TODO fix seq_along
  # sapply(pos_list, function(x) {
  #   ifelse(is_named(x) & length(x)>0, names(x), seq_along(x))
  # })
}
