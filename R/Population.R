#' Population Class
#' @description R6 class managing a collection of \code{Chromosome} objects.
#'   Provides population-level initialization, fitness evaluation, and
#'   best-individual retrieval.
#' @field individuals List of \code{Chromosome} objects.
#' @field size Population size.
#' @export
Population <- R6::R6Class(
  "Population",
  public = list(
    individuals = NULL,
    size        = NULL,

    #' @description Initialize a population of chromosomes.
    #' @param size Population size.
    #' @param n_genes Number of genes per chromosome.
    #' @param type Encoding type: \code{"binary"} (default) or \code{"real"}.
    #' @param lower Lower bound for real-valued genes (required when
    #'   \code{type = "real"}).
    #' @param upper Upper bound for real-valued genes (required when
    #'   \code{type = "real"}).
    #' @param ... Additional arguments (unused, reserved for subclasses).
    initialize = function(size, n_genes, type = "binary",
                          lower = NULL, upper = NULL, ...) {
      self$size <- size
      if (type == "binary") {
        self$individuals <- lapply(seq_len(size), function(i) {
          BinaryChromosome$new(sample(c(0L, 1L), n_genes, replace = TRUE))
        })
      } else {
        if (is.null(lower) || is.null(upper)) {
          stop("'lower' and 'upper' must be provided when type = \"real\".")
        }
        lower <- rep_len(lower, n_genes)
        upper <- rep_len(upper, n_genes)
        self$individuals <- lapply(seq_len(size), function(i) {
          RealChromosome$new(stats::runif(n_genes, lower, upper))
        })
      }
    },

    #' @description Evaluate fitness for all individuals.
    #' @param fitness_fn Fitness function; receives a gene vector and returns a
    #'   single numeric value.
    #' @param parallel Logical; if \code{TRUE}, evaluation is parallelized
    #'   (currently delegates to sequential evaluation — use \code{run_ga()}
    #'   with \code{parallel = TRUE} for parallel support).
    evaluate = function(fitness_fn, parallel = FALSE) {
      for (ind in self$individuals) {
        ind$fitness <- fitness_fn(ind$genes)
      }
      invisible(self)
    },

    #' @description Get the individual with the highest fitness.
    #' @return The \code{Chromosome} object with the maximum fitness value.
    get_best = function() {
      fitnesses <- vapply(self$individuals,
                          function(ind) ind$fitness,
                          numeric(1))
      self$individuals[[which.max(fitnesses)]]
    }
  )
)
