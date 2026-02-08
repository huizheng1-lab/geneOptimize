#' Functional interface for Genetic Algorithm
#' @description A high-level wrapper to run the Genetic Algorithm.
#' @param fitness_fn Fitness function.
#' @param n_genes Number of genes.
#' @param pop_size Population size.
#' @param generations Generations.
#' @param selection_fn Selection operator object (R6).
#' @param crossover_fn Crossover operator object (R6).
#' @param mutation_fn Mutation operator object (R6).
#' @param crossover_rate Crossover rate.
#' @param mutation_rate Mutation rate.
#' @param type Encoding type.
#' @param elitism_count Elitism count.
#' @param verbose Verbose output.
#' @param parallel Parallel execution.
#' @param cores Number of cores.
#' @param local_search_fn Optional local search function for Memetic Algorithms.
#' @param track_diversity Track population diversity over generations (default: FALSE).
#' @param ... Additional arguments.
#' @return Result object of class \code{gene_opt_res}.
#' @export
run_ga <- function(fitness_fn, n_genes, pop_size = 50, generations = 100, 
                  selection_fn = SelectionTournament$new(), 
                  crossover_fn = CrossoverSinglePoint$new(), 
                  mutation_fn = MutationSimple$new(), 
                  crossover_rate = 0.8, mutation_rate = 0.01, 
                  type = "binary", elitism_count = 1, verbose = TRUE, parallel = FALSE, 
                  cores = 1, local_search_fn = NULL, 
                  track_diversity = FALSE, ...) {
  
  if (type == "binary") {
    population <- matrix(sample(c(0, 1), pop_size * n_genes, replace = TRUE), nrow = pop_size)
  } else {
    args <- list(...)
    population <- matrix(stats::runif(pop_size * n_genes, args$lower, args$upper), nrow = pop_size)
  }
  
  best_history <- numeric(generations)
  mean_history <- numeric(generations)
  worst_history <- numeric(generations)
  diversity_history <- numeric(generations)
  
  for (gen in 1:generations) {
    # Fitness Evaluation
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
    mean_history[gen] <- mean(fitness_values)
    worst_history[gen] <- min(fitness_values)
    
    # Track diversity (for binary: Hamming distance; for real: Euclidean)
    if (track_diversity && gen > 1) {
      if (type == "binary") {
        # Hamming distance based diversity
        div_matrix <- outer(1:pop_size, 1:pop_size, Vectorize(function(i, j) {
          sum(population[i, ] != population[j, ])
        }))
        diversity_history[gen] <- mean(div_matrix[lower.tri(div_matrix)])
      } else {
        # Euclidean distance based diversity
        center <- colMeans(population)
        div_matrix <- outer(1:pop_size, 1:pop_size, Vectorize(function(i, j) {
          sqrt(sum((population[i, ] - population[j, ])^2))
        }))
        diversity_history[gen] <- mean(div_matrix[lower.tri(div_matrix)])
      }
    } else if (track_diversity) {
      diversity_history[gen] <- 0
    }
    
    if (verbose) {
      cat(sprintf("Gen %d: Best = %f, Mean = %f\n", gen, current_best_fitness, mean_history[gen]))
    }
    
    # Elitism
    order_idx <- order(fitness_values, decreasing = TRUE)
    elites <- population[order_idx[1:elitism_count], , drop = FALSE]
    
    # Selection
    new_pop <- matrix(0, nrow = pop_size, ncol = n_genes)
    for (i in 1:pop_size) {
      new_pop[i, ] <- selection_fn$select(population, fitness_values, ...)
    }
    
    # Crossover
    for (i in seq(1, pop_size - 1, by = 2)) {
      if (stats::runif(1) < crossover_rate) {
        children <- crossover_fn$mate(new_pop[i, ], new_pop[i + 1, ])
        new_pop[i, ] <- children[[1]]
        new_pop[i + 1, ] <- children[[2]]
      }
    }
    
    # Mutation
    for (i in 1:pop_size) {
      new_pop[i, ] <- mutation_fn$mutate(new_pop[i, ], type, mutation_rate, ...)
    }
    
    # Local Search (Memetic Hybridization)
    if (!is.null(local_search_fn)) {
      for (i in 1:pop_size) {
        new_pop[i, ] <- local_search_fn(new_pop[i, ])
      }
    }
    
    # Replacement with Elites
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
  
  # Add extended statistics
  res$mean_history <- mean_history
  res$worst_history <- worst_history
  if (track_diversity) {
    res$diversity_history <- diversity_history
  }
  
  return(res)
}
