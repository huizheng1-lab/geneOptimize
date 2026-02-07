#' Single-point crossover for vectors
#' @param parent1 First parent vector.
#' @param parent2 Second parent vector.
#' @return A list of two offspring vectors.
#' @export
crossover_single_point <- function(parent1, parent2) {
  n_genes <- length(parent1)
  point <- sample(1:(n_genes - 1), 1)
  child1 <- c(parent1[1:point], parent2[(point + 1):n_genes])
  child2 <- c(parent2[1:point], parent1[(point + 1):n_genes])
  return(list(child1, child2))
}

#' Uniform crossover for vectors
#' @param parent1 First parent vector.
#' @param parent2 Second parent vector.
#' @return A list of two offspring vectors.
#' @export
crossover_uniform <- function(parent1, parent2) {
  n_genes <- length(parent1)
  mask <- sample(c(0, 1), n_genes, replace = TRUE)
  child1 <- ifelse(mask == 1, parent1, parent2)
  child2 <- ifelse(mask == 1, parent2, parent1)
  return(list(child1, child2))
}

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
      crossover_single_point(p1, p2)
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
      crossover_uniform(p1, p2)
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

#' Binary mutation (bit-flip)
#' @param chromosome Binary vector.
#' @param mutation_rate Probability of flipping each bit.
#' @param ... Additional arguments.
#' @return Mutated binary vector.
#' @export
mutation_binary <- function(chromosome, mutation_rate, ...) {
  n_genes <- length(chromosome)
  mask <- stats::runif(n_genes) < mutation_rate
  chromosome[mask] <- 1 - chromosome[mask]
  return(chromosome)
}

#' Real-valued mutation (Gaussian)
#' @param chromosome Numeric vector.
#' @param mutation_rate Probability of mutating each gene.
#' @param lower Lower bound.
#' @param upper Upper bound.
#' @param ... Additional arguments.
#' @return Mutated numeric vector.
#' @export
mutation_real <- function(chromosome, mutation_rate, lower, upper, ...) {
  n_genes <- length(chromosome)
  mask <- stats::runif(n_genes) < mutation_rate
  noise <- stats::rnorm(sum(mask), mean = 0, sd = (upper - lower) / 10)
  chromosome[mask] <- chromosome[mask] + noise
  return(pmax(pmin(chromosome, upper), lower))
}

#' Mutation Operator Base Class
#' @description Base R6 class for mutation operators.
#' @export
MutationOperator <- R6::R6Class(
  "MutationOperator",
  public = list(
    #' @description Mutate genes.
    #' @param genes Gene vector.
    #' @param type Type.
    #' @param rate Rate.
    mutate = function(genes, type, rate) {
      stop("Not implemented")
    }
  )
)

#' Simple Mutation Class
#' @description R6 class for standard mutation.
#' @export
MutationSimple <- R6::R6Class(
  "MutationSimple",
  inherit = MutationOperator,
  public = list(
    #' @description Mutate genes.
    #' @param genes Gene vector.
    #' @param type Type.
    #' @param rate Rate.
    mutate = function(genes, type, rate) {
      if (type == "binary") mutation_binary(genes, rate)
      else mutation_real(genes, rate)
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
    mutate = function(genes, rate, sigma, lower, upper) {
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
    #' @param pop Population matrix.
    #' @param n Number to select.
    select = function(pop, n) {
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
    k = 2,
    #' @description Initialize.
    #' @param k Tournament size (default 2).
    initialize = function(k = 2) {
      self$k <- k
    },
    #' @description Select.
    #' @param pop Population.
    #' @param n Number.
    select = function(pop, n) {
      # Logic
    }
  )
)

#' Roulette wheel selection
#' @param population Population matrix.
#' @param fitness_values Vector of fitness values.
#' @param ... Additional arguments.
#' @return Selected individual vector.
#' @export
selection_roulette <- function(population, fitness_values, ...) {
  pop_size <- nrow(population)
  min_fit <- min(fitness_values)
  adj_fitness <- if (min_fit < 0) fitness_values - min_fit + 0.01 else fitness_values
  probs <- adj_fitness / sum(adj_fitness)
  idx <- sample(1:pop_size, 1, prob = probs)
  return(population[idx, ])
}

#' Tournament selection
#' @param population Population matrix.
#' @param fitness_values Vector of fitness values.
#' @param k Tournament size (default 3).
#' @param ... Additional arguments.
#' @return Selected individual vector.
#' @export
selection_tournament <- function(population, fitness_values, k = 3, ...) {
  pop_size <- nrow(population)
  indices <- sample(1:pop_size, k)
  winner <- indices[which.max(fitness_values[indices])]
  return(population[winner, ])
}
