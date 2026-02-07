#' Functional interface for Genetic Algorithm
#' @param fitness_fn Fitness function.
#' @param n_genes Number of genes.
#' @param pop_size Population size.
#' @param generations Generations.
#' @param selection_fn Selection function.
#' @param crossover_fn Crossover function.
#' @param mutation_fn Mutation function.
#' @param crossover_rate Crossover rate.
#' @param mutation_rate Mutation rate.
#' @param type Encoding type.
#' @param elitism_count Elitism count.
#' @param verbose Verbose output.
#' @param parallel Parallel execution.
#' @param cores Number of cores.
#' @param ... Additional arguments.
#' @return Result object.
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
