read_mixcr <- function(path) {
  mixcr_column_type <- cols(
    .default = col_logical(),
    cloneId = col_double(),
    cloneCount = col_double(),
    cloneFraction = col_double(),
    targetSequences = col_character(),
    targetQualities = col_character(),
    allVHitsWithScore = col_character(),
    allDHitsWithScore = col_character(),
    allJHitsWithScore = col_character(),
    allCHitsWithScore = col_character(),
    allVAlignments = col_character(),
    allDAlignments = col_character(),
    allJAlignments = col_character(),
    allCAlignments = col_character(),
    nSeqCDR3 = col_character(),
    minQualCDR3 = col_double(),
    aaSeqCDR3 = col_character(),
    refPoints = col_character()
  )

  read_tsv(path, col_types = mixcr_column_type)
}
