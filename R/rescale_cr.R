rescale_cr <- function(cr_level) {
  datawizard::rescale(cr_level, to = c(0, 1), select = tidyselect::where(is.numeric))
}

# Difference between centering and standardizing: Standardized variables are computed by subtracting the mean of the variable and then dividing it by the standard deviation, while centering variables involves only the subtraction.
center_cr <- function(cr_level) {
  datawizard::center(cr_level, center = 0, select = tidyselect::where(is.numeric))
}


demean_cr <- function(cr_level) {
  datawizard::standardise(cr_level, select = tidyselect::where(is.numeric))
}


rescale_cr <- function(cr_level) {
  datawizard::normalize(cr_level, select = tidyselect::where(is.numeric))
}

cr_list$inv.freq
demean(cr_level, select = {{aa}}, group = {{id}})
demean_cr <- function(level, aa, id) {
  demean(level, select = {{aa}}, group = {{id}})
}

categorize(cr_list$inv.freq)

categorize(cr_list$inv.freq, split = "quantile", n_groups=3, label=c("low", "mid", "high"))

x$n <- cr_level(x, nt, aa)
datawizard::reshape_wider(x, id_cols = "aa", values_from = "n", names_from = 'id')
(tapply())



insight::n_unique()
