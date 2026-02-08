#' Base Chromosome Class
#' @description An abstract R6 class representing a chromosome.
#' @field genes The genetic material.
#' @field fitness The fitness value.
#' @field type The encoding type.
#' @export
Chromosome <- R6::R6Class(
  "Chromosome",
  public = list(
    genes = NULL,
    fitness = -Inf,
    type = NULL,
    initialize = function(genes, type = "binary") {
      self$genes <- genes
      self$type <- type
    },
    mutate = function(mutation_rate, ...) {
      stop("Method 'mutate' must be implemented by subclasses.")
    },
    copy = function() {
      new_chrom <- self$clone(deep = TRUE)
      return(new_chrom)
    }
  )
)

#' Binary Chromosome Class
#' @description R6 class representing a binary chromosome.
#' @export
BinaryChromosome <- R6::R6Class(
  "BinaryChromosome",
  inherit = Chromosome,
  public = list(
    initialize = function(genes) {
      super$initialize(genes, type = "binary")
    },
    mutate = function(mutation_rate, ...) {
      n_genes <- length(self$genes)
      mask <- stats::runif(n_genes) < mutation_rate
      self$genes[mask] <- 1 - self$genes[mask]
      self$fitness <- -Inf
      invisible(self)
    }
  )
)

#' Real-Valued Chromosome Class
#' @description R6 class representing a real-valued chromosome.
#' @export
RealChromosome <- R6::R6Class(
  "RealChromosome",
  inherit = Chromosome,
  public = list(
    initialize = function(genes) {
      super$initialize(genes, type = "real")
    },
    mutate = function(mutation_rate, lower, upper, ...) {
      n_genes <- length(self$genes)
      mask <- stats::runif(n_genes) < mutation_rate
      noise <- stats::rnorm(sum(mask), mean = 0, sd = (upper - lower) / 10)
      self$genes[mask] <- self$genes[mask] + noise
      self$genes <- pmax(pmin(self$genes, upper), lower)
      self$fitness <- -Inf
      invisible(self)
    }
  )
)
