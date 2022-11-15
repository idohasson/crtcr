# add packages dependencies
for (p in c("rlang", "dplyr", "purrr", "magrittr"))
  {do.call(use_package, list(p))}
