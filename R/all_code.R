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
    copy = function() {
      new_chrom <- Chromosome$new(self$genes, self$type)
      new_chrom$fitness <- self$fitness
      return(new_chrom)
    }
  )
)

#' @export
crossover_single_point <- function(parent1, parent2) {
  n_genes <- length(parent1)
  point <- sample(1:(n_genes - 1), 1)
  child1 <- c(parent1[1:point], parent2[(point + 1):n_genes])
  child2 <- c(parent2[1:point], parent1[(point + 1):n_genes])
  return(list(child1, child2))
}

#' @export
crossover_uniform <- function(parent1, parent2) {
  n_genes <- length(parent1)
  mask <- sample(c(0, 1), n_genes, replace = TRUE)
  child1 <- ifelse(mask == 1, parent1, parent2)
  child2 <- ifelse(mask == 1, parent2, parent1)
  return(list(child1, child2))
}

#' @export
CrossoverOperator <- R6::R6Class(
  "CrossoverOperator",
  public = list(
    mate = function(p1, p2) {
      stop("Not implemented")
    }
  )
)

#' @export
CrossoverSinglePoint <- R6::R6Class(
  "CrossoverSinglePoint",
  inherit = CrossoverOperator,
  public = list(
    mate = function(p1, p2) {
      crossover_single_point(p1, p2)
    }
  )
)

#' @export
CrossoverUniform <- R6::R6Class(
  "CrossoverUniform",
  inherit = CrossoverOperator,
  public = list(
    mate = function(p1, p2) {
      crossover_uniform(p1, p2)
    }
  )
)

#' @export
GeneticAlgorithm <- R6::R6Class(
  "GeneticAlgorithm",
  public = list(
    pop = NULL,
    generations = NULL,
    crossover_rate = NULL,
    mutation_rate = NULL,
    fitness_fn = NULL,
    history = NULL,
    operators = list(),
    initialize = function(pop, fitness_fn, generations = 100, crossover_rate = 0.8, mutation_rate = 0.01) {
      self$pop <- pop
      self$fitness_fn <- fitness_fn
      self$generations <- generations
      self$crossover_rate <- crossover_rate
      self$mutation_rate <- mutation_rate
    },
    run = function(verbose = TRUE, parallel = FALSE) {
      # Implementation details omitted for brevity or reconstructed from run_ga
    }
  )
)

#' @export
mutation_binary <- function(chromosome, mutation_rate, ...) {
  n_genes <- length(chromosome)
  mask <- stats::runif(n_genes) < mutation_rate
  chromosome[mask] <- 1 - chromosome[mask]
  return(chromosome)
}

#' @export
mutation_real <- function(chromosome, mutation_rate, lower, upper, ...) {
  n_genes <- length(chromosome)
  mask <- stats::runif(n_genes) < mutation_rate
  noise <- stats::rnorm(sum(mask), mean = 0, sd = (upper - lower) / 10)
  chromosome[mask] <- chromosome[mask] + noise
  return(pmax(pmin(chromosome, upper), lower))
}

#' @export
MutationOperator <- R6::R6Class(
  "MutationOperator",
  public = list(
    mutate = function(genes, type, rate) {
      stop("Not implemented")
    }
  )
)

#' @export
MutationSimple <- R6::R6Class(
  "MutationSimple",
  inherit = MutationOperator,
  public = list(
    mutate = function(genes, type, rate) {
      if (type == "binary") {
        mutation_binary(genes, rate)
      } else {
        mutation_real(genes, rate)
      }
    }
  )
)

#' @export
new_gene_opt_res <- function(best_chromosome, best_fitness, history, call) {
  structure(list(
    best_chromosome = best_chromosome,
    best_fitness = best_fitness,
    history = history,
    call = call
  ), class = "gene_opt_res")
}

#' @export
Population <- R6::R6Class(
  "Population",
  public = list(
    individuals = NULL,
    size = NULL,
    initialize = function(size, n_genes, type = "binary", ...) {
      self$size <- size
      # Initialization logic
    },
    evaluate = function(fitness_fn, parallel = FALSE) {
      # Evaluation logic
    },
    get_best = function() {
      # Best logic
    }
  )
)

#' @export
run_ga <- function(fitness_fn, n_genes, pop_size = 50, generations = 100, 
                  selection_fn = selection_tournament, crossover_fn = crossover_single_point, 
                  mutation_fn = mutation_binary, crossover_rate = 0.8, mutation_rate = 0.01, 
                  type = "binary", elitism_count = 1, verbose = TRUE, parallel = FALSE, 
                  cores = 1, ...) {
  if (type == "binary") {
    population <- matrix(sample(c(0, 1), pop_size * n_genes, replace = TRUE), nrow = pop_size)
  } else {
    args <- list(...)
    population <- matrix(stats::runif(pop_size * n_genes, args$lower, args$upper), nrow = pop_size)
  }
  best_history <- numeric(generations)
  for (gen in 1:generations) {
    if (parallel && cores > 1) {
      if (.Platform$OS.type == "unix") {
        fitness_values <- unlist(parallel::mclapply(seq_len(nrow(population)), 
                                                   function(i) fitness_fn(population[i, ]), mc.cores = cores))
      } else {
        cl <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl))
        fitness_values <- parallel::parApply(cl, population, 1, fitness_fn)
      }
    } else {
      fitness_values <- apply(population, 1, fitness_fn)
    }
    best_idx <- which.max(fitness_values)
    current_best_fitness <- fitness_values[best_idx]
    best_history[gen] <- current_best_fitness
    order_idx <- order(fitness_values, decreasing = TRUE)
    elites <- population[order_idx[1:elitism_count], , drop = FALSE]
    if (verbose) {
      cat(sprintf("Gen %d: Best = %f\n", gen, current_best_fitness))
    }
    new_pop <- matrix(0, nrow = pop_size, ncol = n_genes)
    for (i in 1:pop_size) {
      new_pop[i, ] <- selection_fn(population, fitness_values, ...)
    }
    for (i in seq(1, pop_size - 1, by = 2)) {
      if (stats::runif(1) < crossover_rate) {
        children <- crossover_fn(new_pop[i, ], new_pop[i + 1, ])
        new_pop[i, ] <- children[[1]]
        new_pop[i + 1, ] <- children[[2]]
      }
    }
    for (i in 1:pop_size) {
      new_pop[i, ] <- mutation_fn(new_pop[i, ], mutation_rate, ...)
    }
    new_pop[1:elitism_count, ] <- elites
    population <- new_pop
  }
  final_fitness <- apply(population, 1, fitness_fn)
  best_final_idx <- which.max(final_fitness)
  res <- new_gene_opt_res(
    best_chromosome = population[best_final_idx, ],
    best_fitness = final_fitness[best_final_idx],
    history = best_history,
    call = match.call()
  )
  return(res)
}

#' @export
selection_roulette <- function(population, fitness_values, ...) {
  pop_size <- nrow(population)
  min_fit <- min(fitness_values)
  adj_fitness <- if (min_fit < 0) fitness_values - min_fit + 0.01 else fitness_values
  probs <- adj_fitness / sum(adj_fitness)
  idx <- sample(1:pop_size, 1, prob = probs)
  return(population[idx, ])
}

#' @export
selection_tournament <- function(population, fitness_values, k = 3, ...) {
  pop_size <- nrow(population)
  indices <- sample(1:pop_size, k)
  winner <- indices[which.max(fitness_values[indices])]
  return(population[winner, ])
}

#' @export
SelectionOperator <- R6::R6Class(
  "SelectionOperator",
  public = list(
    select = function(pop, n) {
      stop("Not implemented")
    }
  )
)

#' @export
SelectionTournament <- R6::R6Class(
  "SelectionTournament",
  inherit = SelectionOperator,
  public = list(
    k = 2,
    initialize = function(k = 2) {
      self$k <- k
    },
    select = function(pop, n) {
      # Selection logic
    }
  )
)
