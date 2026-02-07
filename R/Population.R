#' Population Class
#' @description R6 class managing a collection of Chromosome objects.
#' @field individuals List of Chromosome objects.
#' @field size Population size.
#' @export
Population <- R6::R6Class(
  "Population",
  public = list(
    individuals = NULL,
    size = NULL,
    #' @description Initialize a population.
    #' @param size Population size.
    #' @param n_genes Number of genes per chromosome.
    #' @param type Encoding type ("binary" or "real").
    #' @param ... Additional arguments for initialization.
    initialize = function(size, n_genes, type = "binary", ...) {
      self$size <- size
    },
    #' @description Evaluate fitness for all individuals.
    #' @param fitness_fn The fitness function to apply.
    #' @param parallel Logical; if TRUE, evaluation is parallelized.
    evaluate = function(fitness_fn, parallel = FALSE) {
    },
    #' @description Get the best individual in the population.
    #' @return The Chromosome object with the highest fitness.
    get_best = function() {
    }
  )
)
