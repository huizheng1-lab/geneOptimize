#' Base Chromosome Class
#' @description An abstract R6 class representing a chromosome in the genetic algorithm.
#' @field genes The genetic material (vector).
#' @field fitness The fitness value of the chromosome.
#' @field type The encoding type ("binary" or "real").
#' @export
Chromosome <- R6::R6Class(
  "Chromosome",
  public = list(
    genes = NULL,
    fitness = -Inf,
    type = NULL,

    #' @description Initialize a new chromosome.
    #' @param genes Initial gene vector.
    #' @param type Encoding type.
    initialize = function(genes, type = "binary") {
      self$genes <- genes
      self$type <- type
    },

    #' @description Abstract mutation method.
    mutate = function(mutation_rate, ...) {
      stop("Method 'mutate' must be implemented by subclasses.")
    },

    #' @description Create a deep copy of the chromosome.
    copy = function() {
      new_chrom <- self$clone(deep = TRUE)
      return(new_chrom)
    }
  )
)

#' Binary Chromosome Class
#' @description R6 class representing a binary-encoded chromosome.
#' @inherit Chromosome
#' @export
BinaryChromosome <- R6::R6Class(
  "BinaryChromosome",
  inherit = Chromosome,
  public = list(
    #' @description Initialize a binary chromosome.
    initialize = function(genes) {
      super$initialize(genes, type = "binary")
    },

    #' @description Perform bit-flip mutation.
    mutate = function(mutation_rate, ...) {
      n_genes <- length(self$genes)
      mask <- stats::runif(n_genes) < mutation_rate
      self$genes[mask] <- 1 - self$genes[mask]
      self$fitness <- -Inf # Reset fitness after mutation
      invisible(self)
    }
  )
)

#' Real-Valued Chromosome Class
#' @description R6 class representing a real-valued chromosome.
#' @inherit Chromosome
#' @export
RealChromosome <- R6::R6Class(
  "RealChromosome",
  inherit = Chromosome,
  public = list(
    #' @description Initialize a real chromosome.
    initialize = function(genes) {
      super$initialize(genes, type = "real")
    },

    #' @description Perform Gaussian mutation.
    mutate = function(mutation_rate, lower, upper, ...) {
      n_genes <- length(self$genes)
      mask <- stats::runif(n_genes) < mutation_rate
      # Scale noise to 10% of the range as suggested in the paper
      noise <- stats::rnorm(sum(mask), mean = 0, sd = (upper - lower) / 10)
      self$genes[mask] <- self$genes[mask] + noise
      # Clamp to bounds
      self$genes <- pmax(pmin(self$genes, upper), lower)
      self$fitness <- -Inf
      invisible(self)
    }
  )
)
