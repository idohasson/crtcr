
# Read file ---------------------------

read_file <- function(path, ...) {

  data.table::fread(path.expand(path), ...)

}

read_mixcr <- function(path) {

  CDR3NT="nSeqCDR3"
  CDR3AA="aaSeqCDR3"

  data.table::fread(path.expand(path), select = c(CDR3NT, CDR3AA))

}

