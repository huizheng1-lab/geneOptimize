#' Crossover Operator Base Class
#' @description Base R6 class for crossover operators.
#' @export
CrossoverOperator <- R6::R6Class(
  "CrossoverOperator",
  public = list(
    #' @description Mate two parents.
    #' @param p1 Parent 1.
    #' @param p2 Parent 2.
    mate = function(p1, p2) {
      stop("Not implemented")
    }
  )
)

#' Single-Point Crossover Class
#' @description R6 class for single-point crossover.
#' @export
CrossoverSinglePoint <- R6::R6Class(
  "CrossoverSinglePoint",
  inherit = CrossoverOperator,
  public = list(
    #' @description Mate two parents.
    #' @param p1 Parent 1.
    #' @param p2 Parent 2.
    mate = function(p1, p2) {
      n_genes <- length(p1)
      point <- sample(1:(n_genes - 1), 1)
      child1 <- c(p1[1:point], p2[(point + 1):n_genes])
      child2 <- c(p2[1:point], p1[(point + 1):n_genes])
      return(list(child1, child2))
    }
  )
)

#' Uniform Crossover Class
#' @description R6 class for uniform crossover.
#' @export
CrossoverUniform <- R6::R6Class(
  "CrossoverUniform",
  inherit = CrossoverOperator,
  public = list(
    #' @description Mate two parents.
    #' @param p1 Parent 1.
    #' @param p2 Parent 2.
    mate = function(p1, p2) {
      n_genes <- length(p1)
      mask <- sample(c(0, 1), n_genes, replace = TRUE)
      child1 <- ifelse(mask == 1, p1, p2)
      child2 <- ifelse(mask == 1, p2, p1)
      return(list(child1, child2))
    }
  )
)

#' Arithmetic Crossover Class
#' @description R6 class for arithmetic (real-valued) crossover.
#' @field alpha Mixing parameter.
#' @export
CrossoverArithmetic <- R6::R6Class(
  "CrossoverArithmetic",
  inherit = CrossoverOperator,
  public = list(
    alpha = 0.5,
    #' @description Initialize.
    #' @param alpha Mixing parameter (default 0.5).
    initialize = function(alpha = 0.5) {
      self$alpha <- alpha
    },
    #' @description Mate two parents.
    #' @param p1 Parent 1.
    #' @param p2 Parent 2.
    mate = function(p1, p2) {
      c1 <- self$alpha * p1 + (1 - self$alpha) * p2
      c2 <- (1 - self$alpha) * p1 + self$alpha * p2
      return(list(c1, c2))
    }
  )
)

#' Mutation Operator Base Class
#' @description Base R6 class for mutation operators.
#' @export
MutationOperator <- R6::R6Class(
  "MutationOperator",
  public = list(
    #' @description Mutate genes.
    #' @param genes Gene vector.
    #' @param type Encoding type.
    #' @param rate Mutation rate.
    #' @param ... Additional arguments.
    mutate = function(genes, type, rate, ...) {
      stop("Not implemented")
    }
  )
)

#' Simple Mutation Class
#' @description R6 class for standard mutation (Binary bit-flip or Real noise).
#' @export
MutationSimple <- R6::R6Class(
  "MutationSimple",
  inherit = MutationOperator,
  public = list(
    #' @description Mutate genes.
    #' @param genes Gene vector.
    #' @param type Type.
    #' @param rate Rate.
    #' @param ... Additional arguments.
    mutate = function(genes, type, rate, ...) {
      if (type == "binary") {
        n_genes <- length(genes)
        mask <- stats::runif(n_genes) < rate
        genes[mask] <- 1 - genes[mask]
        return(genes)
      } else {
        args <- list(...)
        n_genes <- length(genes)
        mask <- stats::runif(n_genes) < rate
        noise <- stats::rnorm(sum(mask), mean = 0, sd = (args$upper - args$lower) / 10)
        genes[mask] <- genes[mask] + noise
        return(pmax(pmin(genes, args$upper), args$lower))
      }
    }
  )
)

#' Gaussian Mutation Class
#' @description R6 class for Gaussian mutation.
#' @export
MutationGaussian <- R6::R6Class(
  "MutationGaussian",
  inherit = MutationOperator,
  public = list(
    #' @description Mutate genes.
    #' @param genes Gene vector.
    #' @param rate Rate.
    #' @param sigma Sigma.
    #' @param lower Lower bound.
    #' @param upper Upper bound.
    #' @param ... Additional arguments.
    mutate = function(genes, rate, sigma, lower, upper, ...) {
       mask <- stats::runif(length(genes)) < rate
       noise <- stats::rnorm(sum(mask), mean = 0, sd = sigma)
       genes[mask] <- genes[mask] + noise
       return(pmax(pmin(genes, upper), lower))
    }
  )
)

#' Selection Operator Base Class
#' @description Base R6 class for selection.
#' @export
SelectionOperator <- R6::R6Class(
  "SelectionOperator",
  public = list(
    #' @description Select individuals.
    #' @param population Population matrix.
    #' @param fitness_values Vector of fitness values.
    #' @param n Number to select.
    #' @param ... Additional arguments.
    select = function(population, fitness_values, n = 1, ...) {
      stop("Not implemented")
    }
  )
)

#' Tournament Selection Class
#' @description R6 class for tournament selection.
#' @field k Tournament size.
#' @export
SelectionTournament <- R6::R6Class(
  "SelectionTournament",
  inherit = SelectionOperator,
  public = list(
    k = 3,
    #' @description Initialize.
    #' @param k Tournament size (default 3).
    initialize = function(k = 3) {
      self$k <- k
    },
    #' @description Select an individual.
    #' @param population Population matrix.
    #' @param fitness_values Fitness values.
    #' @param n Number (ignored for single selection).
    #' @param ... Additional arguments.
    select = function(population, fitness_values, n = 1, ...) {
      pop_size <- nrow(population)
      indices <- sample(1:pop_size, self$k)
      winner <- indices[which.max(fitness_values[indices])]
      return(population[winner, ])
    }
  )
)

#' Roulette Selection Class
#' @description R6 class for roulette wheel selection.
#' @export
SelectionRoulette <- R6::R6Class(
  "SelectionRoulette",
  inherit = SelectionOperator,
  public = list(
    #' @description Select an individual.
    #' @param population Population matrix.
    #' @param fitness_values Fitness values.
    #' @param n Number.
    #' @param ... Additional arguments.
    select = function(population, fitness_values, n = 1, ...) {
      pop_size <- nrow(population)
      min_fit <- min(fitness_values)
      adj_fitness <- if (min_fit < 0) fitness_values - min_fit + 0.01 else fitness_values
      probs <- adj_fitness / sum(adj_fitness)
      idx <- sample(1:pop_size, 1, prob = probs)
      return(population[idx, ])
    }
  )
)
