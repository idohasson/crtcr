# is_super_public(x)
# condition(x, y, .condition = x>=2 & y<4)
is_super_public <-  function(cr_count) {
  super_min_freq <- ~ top_prec(.x, top = .5)
  level_values <- level(cr_count, super_min_freq)
  condition(cr=level_values, .condition = cr != 0)
}
