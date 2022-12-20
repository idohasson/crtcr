#' Calculate the level of sharing of a given clonotype among different TCR repertoires
#'
#' @param repertoire_list a list of data frames, each containing a TCR repertoire with columns for the clonotype and the sequence
#' @param clonotype a character string specifying the clonotype to calculate the level of sharing for
#' @return a numeric value representing the level of sharing of the specified clonotype among the different TCR repertoires
#' @export
calculate_shared_clonotypes <- function(repertoire_list, clonotype) {
  # Initialize a count of the number of repertoires that contain the clonotype
  clonotype_count <- 0

  # Iterate through each repertoire in the list
  for (repertoire in repertoire_list) {
    # Check if the clonotype is present in the current repertoire
    clonotype_present <- clonotype %in% repertoire$clonotype

    # If the clonotype is present, increment the count
    if (clonotype_present) {
      clonotype_count <- clonotype_count + 1
    }
  }

  # Calculate the level of sharing as the number of repertoires that contain the clonotype divided by the total number of repertoires
  sharing_level <- clonotype_count / length(repertoire_list)

  # Return the sharing level as output
  return(sharing_level)
}

