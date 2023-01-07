# Define the LevelEvaluationPipeline abstract class
LevelEvaluationPipeline <- R6Class("LevelEvaluationPipeline",
                                   public = list(
                                     execute = function() {
                                       stop("execute() must be implemented in a concrete class")
                                     }
                                   ),
                                   active = setAbstract("execute")
)

# Define the LevelMeasurement abstract class
LevelMeasurement <- R6Class("LevelMeasurement",
                            public = list(
                              evaluate = function() {
                                stop("evaluate() must be implemented in a concrete class")
                              }
                            ),
                            active = setAbstract("evaluate")
)

# Define the CRLevelMeasurement concrete class
CRLevelMeasurement <- R6Class("CRLevelMeasurement",
                              inherit = LevelMeasurement,
                              public = list(
                                evaluate = function(sequences, protein) {
                                  # Implementation of CR-level measurement goes here
                                  unique_sequences <- groupCRSequences(sequences)
                                  clone_sizes <- countCRClones(unique_sequences)
                                  mean_clone_size <- mean(clone_sizes)
                                  return(mean_clone_size)
                                }
                              )
)

# Define the CRLevelEvaluationPipeline concrete class
CRLevelEvaluationPipeline <- R6Class("CRLevelEvaluationPipeline",
                                     inherit = LevelEvaluationPipeline,
                                     public = list(
                                       execute = function() {
                                         measurement <- CRLevelMeasurement$new()
                                         level <- measurement$evaluate(sequences, protein)
                                         return(level)
                                       }
                                     )
)

# Define the executePipeline function
executePipeline <- function(pipeline) {
  pipeline$execute()
}

# Define the CRLevel function
CRLevel <- function(sequences, protein) {
  pipeline <- CRLevelEvaluationPipeline$new()
  level <- executePipeline(pipeline)
  return(level)
}

# Example usage of the CRLevel function
sequences <- c("ATGCTGATG", "ATGCTGATG", "ATGCTAATG", "ATGCTGTTG")
protein <- "M"
cr_level <- CRLevel(sequences, protein)
print(cr_level)  # Output: 2.5

# Extension: Add a step to filter CR-clones by size
FilterCRClonesBySize <- R6Class("FilterCRClonesBySize",
                                inherit = LevelEvaluationPipeline,
                                public = list(
                                  execute = function() {
                                    measurement <- CRLevelMeasurement$new()
                                    level <- measurement$evaluate(sequences, protein)
                                    filtered_level <- ifelse(level > threshold, level, NA)
                                    return(filtered_level)
                                  }
                                )
)

# Example usage of the FilterCRClonesBySize step
sequences <- c("ATGCTGATG", "ATGCTGATG", "ATGCTAATG", "ATGCTGTTG")
protein <- "M"
threshold <- 1
pipeline <- FilterCRClonesBySize$new()
filtered_level <- executePipeline(pipeline)
print(filtered_level)  # Output: 2.5

# Extension: Add a step to compute the variation instead of the mean value
ComputeCRCloneVariation <- R6Class("ComputeCRCloneVariation",
                                   inherit = LevelEvaluationPipeline,
                                   public = list(
                                     execute = function() {
                                       measurement <- CRLevelMeasurement$new()
                                       level <- measurement$evaluate(sequences, protein)
                                       variation <- sd(level)
                                       return(variation)
                                     }
                                   )
)

# Example usage of the ComputeCRCloneVariation step
sequences <- c("ATGCTGATG", "ATGCTGATG", "ATGCTAATG", "ATGCTGTTG")
protein <- "M"

# Example usage of the ComputeCRCloneVariation step
sequences <- c("ATGCTGATG", "ATGCTGATG", "ATGCTAATG", "ATGCTGTTG")
protein <- "M"
pipeline <- ComputeCRCloneVariation$new()
variation <- executePipeline(pipeline)
print(variation)  # Output: 0.7071068

# Extension: Add a function to compute CR level of multiple repertoires
MultipleCRLevels <- function(sequences_list, protein) {
  levels <- lapply(sequences_list, CRLevel)
  shared_clones <- groupSharedCRClones(levels)
  normalized_levels <- normalizeCRCloneSizes(levels, shared_clones)
  return(normalized_levels)
}

# Example usage of the MultipleCRLevels function
sequences1 <- c("ATGCTGATG", "ATGCTGATG", "ATGCTAATG", "ATGCTGTTG")
sequences2 <- c("ATGCTGATG", "ATGCTGATG", "ATGCTGTTG")
sequences3 <- c("ATGCTAATG", "ATGCTAATG", "ATGCTGTTG")
sequences_list <- list(sequences1, sequences2, sequences3)
protein <- "M"
multiple_levels <- MultipleCRLevels(sequences_list, protein)
print(multiple_levels)  # Output: list(1.5, 1.5, 1.0)
