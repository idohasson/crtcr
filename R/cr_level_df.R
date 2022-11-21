
cr_level_df <- function(.data, .clonal_seq, .clonotype_seq) {




  if (missing(.clonotype_seq)) {

    group_by(.data, clonotype=translate({{.clonal_seq}}), .add = TRUE) %>%

      summarise(CR_level = cr_level({{.clonal_seq}}))

  } else {

    group_by(.data, {{.clonotype_seq}}, .add = TRUE) %>%

      summarise(CR_level = cr_level({{.clonal_seq}}))

  }

}
