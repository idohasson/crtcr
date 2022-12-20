
# f <- function(nt, ...) vec_unique_count(nt)
# level_func(f, group_func("aa"))(x$nt)
level_func <- function(level_func, group_func) {

  level_func <- as_function(level_func)

  function(data) {

    data_fragments <- group_func(data)

    vapply(data_fragments, do.call, numeric(1), what=level_func)

  }

}


level_func <- function(.var, .level_func, group_func) {


  function(data) {

    group_func()

    func <- function(.var, ...) .level_func(.var)

    do.call(func, data)

    # func(data)

    # vapply(data, do.call, numeric(1), what=func)

  }

}
