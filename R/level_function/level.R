# library(vctrs)
# library(rlang)
# NT=rand_nt_vec(100, 1);AA=translate(NT);ID=rep_along(NT, 1:4);FREQ=runif(100);DF=data.frame(nt=NT, aa=AA, id=ID, freq=FREQ)



# values - numerical values
# intervals - numerical integer / funciton or formula to apply on values to use as interval

#' Compute the level of a numeric vector
#'
#' This function computes the level of each element in a numeric vector by using the provided intervals. The intervals can be specified directly as a numeric vector or as a function that returns a numeric vector. If no intervals are provided, the default is to use the median of the values as the interval.
#'
#' @param values a numeric vector
#' @param intervals a numeric vector or a function that returns a numeric vector
#' @return a numeric vector with the levels for each element in values
#' @examples
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5
#' level(c(1, 2, 3, 4, 5))
#'
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the median as the interval
#' level(c(1, 2, 3, 4, 5), median)
#'
#' # Compute the levels for the numbers 1, 2, 3, 4, and 5 using the function mean as the interval
#' level(c(1, 2, 3, 4, 5), mean)
#' @export
level <- function(values, intervals=median) {

  if (is_formula(intervals))

    intervals <- as_function(intervals)

  if (is_function(intervals))

    intervals <- intervals(values)

  findInterval(values, intervals, rightmost.closed = TRUE)

}


function(group_var) {

  function (...) {

    vars <- dots_splice(...)

    dfl <- df_list(..., .name_repair = "minimal")

    data <- new_data_frame(dfl)

    if (!has_name(data, group_var))

      group_var <- which.min(!have_name(data))

    by <- field(data, group_var)

    clonotype_df <- vec_split(data, by)

    tibble::deframe(clonotype_df)

  }

}


# my_level1 <- level_func(vec_unique_count)
# my_level1(NT, AA)


# my_level2 <- level_func(~length(unique(.x)))
# my_level2(NT, aa=AA, id=ID)


# my_level3 <- level_func(mean)
# with(DF, my_level3(freq, aa, id))

level_func <- function(.func, ..., for_each) {

  group_func <- function(group_var) {

    function (...) {

      vars <- dots_splice(...)

      dfl <- df_list(..., .name_repair = "minimal")

      data <- new_data_frame(dfl)

      if (!has_name(data, group_var))

        group_var <- which.max(!have_name(data))

      by <- field(data, group_var)

      clonotype_df <- vec_split(data, by)

      tibble::deframe(clonotype_df)

    }

  }

  .level_func <- as_function(.func)

    # vapply(val, as_function(.func), numeric(1))
    if (!missing(for_each)) {

      .level_func <- function(val, each, ...) {

        group_func(for_each)(val, ..., each)

      }
    }

  .level_func
  # function(values, ..., for_each) {
  #
  #   if (dots_n(...) > 0)
  #
  #     tapply(values, dots_splice(...), level_func)
  #
  #   else
  #
  #     vapply(values, level_func, numeric(1), ...)
  #
  # }

}



# f <- function(nt, ...) vec_unique_count(nt)
# level_func(f, group_func("aa"))(x$nt)
level_func <- function(level_func, group_func) {

  level_func <- as_function(level_func)

  function(data) {

    data_fragments <- group_func(data)

    vapply(data_fragments, do.call, numeric(1), what=level_func)

  }

}

level_var <- function(data, level_func, group_func) {

  data_fragments <- group_func(data)

  vapply(data_fragments, do.call, numeric(1), what=level_func)

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












