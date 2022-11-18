
is_public <- function(.data)
vapply(status, isTRUE, logical(1))

trb <- tcr[tcr$chain=="TRB",]
trb.list <- split(DataFrame(trb), factor(trb$barcode, sce.pbmc$Barcode))length(trb.list)

