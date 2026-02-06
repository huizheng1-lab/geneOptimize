#' Population Class
#' @export
Population <- R6::R6Class("Population",
  public = list(
    individuals = list(),
    size = NULL,
    
    initialize = function(size, n_genes, type = "binary", lower = NULL, upper = NULL) {
      self$size <- size
      for (i in 1:size) {
        if (type == "binary") {
          genes <- sample(c(0, 1), n_genes, replace = TRUE)
        } else {
          genes <- runif(n_genes, lower, upper)
        }
        self$individuals[[i]] <- Chromosome$new(genes, type)
      }
    },

    #' @description Parallelized fitness evaluation using the future framework
    evaluate = function(fitness_fn) {
      # Use future.apply if available, otherwise fallback to lapply
      if (requireNamespace("future.apply", quietly = TRUE)) {
        fitnesses <- future.apply::future_lapply(self$individuals, function(x) fitness_fn(x$genes))
      } else {
        fitnesses <- lapply(self$individuals, function(x) fitness_fn(x$genes))
      }
      
      for (i in seq_along(self$individuals)) {
        self$individuals[[i]]$fitness <- unlist(fitnesses[[i]])
      }
    },

    get_best = function() {
      fitnesses <- sapply(self$individuals, function(x) x$fitness)
      return(self$individuals[[which.max(fitnesses)]])
    }
  )
)

#' GeneticAlgorithm Class
#' @description Orchestrator for the evolutionary cycle, supporting memetic local search.
#' @export
GeneticAlgorithm <- R6::R6Class("GeneticAlgorithm",
  public = list(
    pop = NULL,
    generations = 100,
    p_c = 0.8,
    p_m = 0.01,
    elitism = 1,
    fitness_fn = NULL,
    selection_op = NULL,
    crossover_op = NULL,
    mutation_op = NULL,
    local_search_op = NULL, # Memetic hook
    
    initialize = function(pop, fitness_fn, generations = 100, p_c = 0.8, p_m = 0.01, elitism = 1) {
      self$pop <- pop
      self$fitness_fn <- fitness_fn
      self$generations <- generations
      self$p_c <- p_c
      self$p_m <- p_m
      self$elitism <- elitism
      # Defaults
      self$selection_op <- SelectionTournament$new(k = 2)
      self$crossover_op <- CrossoverUniform$new()
      self$mutation_op <- MutationGaussian$new()
    },

    run = function(verbose = TRUE) {
      for (gen in 1:self$generations) {
        # 1. Evaluate
        self$pop$evaluate(self$fitness_fn)
        best <- self$pop$get_best()
        
        if (verbose) {
          cat(sprintf("Generation %d | Best Fitness: %f\n", gen, best$fitness))
        }

        # 2. Elitism
        fitnesses <- sapply(self$pop$individuals, function(x) x$fitness)
        order_idx <- order(fitnesses, decreasing = TRUE)
        next_gen <- list()
        if (self$elitism > 0) {
          for (i in 1:self$elitism) {
            next_gen[[i]] <- self$pop$individuals[[order_idx[i]]]$copy()
          }
        }

        # 3. Evolutionary Loop
        while (length(next_gen) < self$pop$size) {
          p1 <- self$selection_op$select(self$pop$individuals)
          p2 <- self$selection_op$select(self$pop$individuals)
          
          # Crossover
          if (runif(1) < self$p_c) {
            offspring <- self$crossover_op$mate(p1, p2)
          } else {
            offspring <- list(p1$copy(), p2$copy())
          }
          
          # Mutation & Memetic Search
          for (child in offspring) {
            if (length(next_gen) < self$pop$size) {
              child$genes <- self$mutation_op$mutate(child$genes, self$p_m)
              
              # Local Search (Memetic Hybridization)
              if (!is.null(self$local_search_op)) {
                child$genes <- self$local_search_op(child$genes)
              }
              
              next_gen[[length(next_gen) + 1]] <- child
            }
          }
        }
        self$pop$individuals <- next_gen
      }
      # Final evaluation
      self$pop$evaluate(self$fitness_fn)
      return(self$pop$get_best())
    }
  )
)
