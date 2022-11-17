# clone - empirical derived measurement(s) relating to a TCR or same-sourced T-cells or same genotipic background
# clone-attr - set of categorical values attributed to a clone
# clonotype - unique representation to the composition of a set of clones having the same clone-attr
# repertoire - collection of clones that are not referable to any other collection

# for calculation using either `clone` or `clonotype`,

# discern_clone - distinguish `clone` by its predefined attributes. meaning that
#                 a clone is mapped/tagged to its clonotype, resulting:
#
# by providing either one of:
#   * function - applied to each `clone` variable and return same length categorical values as `clone`.
#   * vector - same length vector as `clone` of comparable 'equal-to' objects.
#   * table object - same number of rows (?) as `clone` so each row corrispond
#                    to all attributes or specified relevant columns.
#   * label - for each clone
# (?) list - clonotype collection as each represent as a set of clones composition.
#
# Output:
# - labeled clonotype vector of the corresponding clone-sets.
#
# `clone_attr` - vector / data frame
# `func` - function producing a scalar value to each clone object.
# ... - func argument

discern_clone.default <- function(clone, clone_attr, func, ...)

discern_clone.default <- function(clone, clone_attr, func, ...)


discern_clone.vector(clone_attr, func=crtcr::translate, ...) {

  if (is.function(func)) {
    func()
  }


}
