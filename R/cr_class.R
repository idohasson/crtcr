
# require(dplyr)
#
# df <- data.frame(x=factor(LETTERS[sample(1:5, 5,replace = TRUE)]),
#                  y=factor(letters[sample(1:2, 5, replace = TRUE)]),
#                  clonotype=sample(c("ASD", "AWD"), 5, replace = TRUE, prob = c(.2,.8)))
#
#
# df %>%
#   group_by(clonotype) %>%
#   summarise(cr=cr_class(.gruop = y, .sub_gruop = x))
#

# cr_class <- function(.gruop, .sub_gruop, ..., .public_class=2, .exclusive_class=1) {
#
#   is_p <- is_public(.gruop, .sub_gruop, ..., .public=.public_class)
#
#   is_e <- is_p & is_exclusive(.sub_gruop, ...,.exclusive = .exclusive_class)
#
#   if (is_e)
#     return("exclusive")
#   else if (is_p)
#     return("inclusive")
#   else
#     return("private")
# }
