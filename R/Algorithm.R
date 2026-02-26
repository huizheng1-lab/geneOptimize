#' Genetic Algorithm Class
#' @description R6 class that orchestrates the evolutionary optimization loop.
#'   This class provides an object-oriented interface to the GA; for most use
#'   cases the functional wrapper \code{\link{run_ga}} is simpler to use.
#' @field pop A \code{\link{Population}} object.
#' @field generations Number of generations.
#' @field crossover_rate Crossover probability.
#' @field mutation_rate Mutation probability.
#' @field fitness_fn Fitness function.
#' @field history Vector of best fitness values, one per completed generation.
#' @field operators Named list of operator objects (reserved for future use).
#' @export
GeneticAlgorithm <- R6::R6Class(
  "GeneticAlgorithm",
  public = list(
    pop           = NULL,
    generations   = NULL,
    crossover_rate = NULL,
    mutation_rate  = NULL,
    fitness_fn    = NULL,
    history       = NULL,
    operators     = list(),

    #' @description Initialize the GeneticAlgorithm.
    #' @param pop A \code{\link{Population}} object.
    #' @param fitness_fn Fitness function.
    #' @param generations Number of generations (default: 100).
    #' @param crossover_rate Crossover probability (default: 0.8).
    #' @param mutation_rate Mutation probability per gene (default: 0.01).
    initialize = function(pop, fitness_fn,
                          generations    = 100,
                          crossover_rate = 0.8,
                          mutation_rate  = 0.01) {
      self$pop           <- pop
      self$fitness_fn    <- fitness_fn
      self$generations   <- generations
      self$crossover_rate <- crossover_rate
      self$mutation_rate  <- mutation_rate
      self$history       <- numeric(0)
    },

    #' @description Run the genetic algorithm.
    #' @param verbose Logical; print progress (default: \code{TRUE}).
    #' @param parallel Logical; parallelize fitness evaluation (default:
    #'   \code{FALSE}).
    #' @return A \code{gene_opt_res} object (invisibly).  The \code{history}
    #'   field of the \code{GeneticAlgorithm} object is also updated.
    run = function(verbose = TRUE, parallel = FALSE) {
      type    <- self$pop$individuals[[1]]$type
      n_genes <- length(self$pop$individuals[[1]]$genes)

      result <- run_ga(
        fitness_fn     = self$fitness_fn,
        n_genes        = n_genes,
        pop_size       = self$pop$size,
        generations    = self$generations,
        crossover_rate = self$crossover_rate,
        mutation_rate  = self$mutation_rate,
        type           = type,
        verbose        = verbose,
        parallel       = parallel
      )

      self$history <- result$history
      invisible(result)
    }
  )
)
