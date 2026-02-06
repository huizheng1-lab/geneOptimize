#' Abstract Operator Base Classes
#' @export
SelectionOperator <- R6::R6Class("SelectionOperator",
  public = list(
    select = function(population, n) stop("Method not implemented")
  )
)

#' @export
CrossoverOperator <- R6::R6Class("CrossoverOperator",
  public = list(
    mate = function(p1, p2) stop("Method not implemented")
  )
)

#' @export
MutationOperator <- R6::R6Class("MutationOperator",
  public = list(
    mutate = function(genes, type, rate, ...) stop("Method not implemented")
  )
)

#' Tournament Selection
#' @export
SelectionTournament <- R6::R6Class("SelectionTournament",
  inherit = SelectionOperator,
  public = list(
    k = 2,
    initialize = function(k = 2) { self$k <- k },
    select = function(population, n) {
      # population is expected to be a list of Chromosomes
      pop_size <- length(population)
      indices <- sample(1:pop_size, self$k)
      # Extract fitnesses
      fitnesses <- sapply(population[indices], function(x) x$fitness)
      winner_idx <- indices[which.max(fitnesses)]
      return(population[[winner_idx]]$copy())
    }
  )
)

#' Uniform Crossover
#' @export
CrossoverUniform <- R6::R6Class("CrossoverUniform",
  inherit = CrossoverOperator,
  public = list(
    mate = function(p1, p2) {
      n_genes <- length(p1$genes)
      mask <- sample(c(0, 1), n_genes, replace = TRUE)
      c1_genes <- ifelse(mask == 1, p1$genes, p2$genes)
      c2_genes <- ifelse(mask == 1, p2$genes, p1$genes)
      return(list(
        Chromosome$new(c1_genes, p1$type),
        Chromosome$new(c2_genes, p1$type)
      ))
    }
  )
)

#' Arithmetic Crossover (Real-Valued)
#' @export
CrossoverArithmetic <- R6::R6Class("CrossoverArithmetic",
  inherit = CrossoverOperator,
  public = list(
    alpha = 0.5,
    initialize = function(alpha = 0.5) { self$alpha <- alpha },
    mate = function(p1, p2) {
      c1_genes <- self$alpha * p1$genes + (1 - self$alpha) * p2$genes
      c2_genes <- (1 - self$alpha) * p1$genes + self$alpha * p2$genes
      return(list(
        Chromosome$new(c1_genes, "real"),
        Chromosome$new(c2_genes, "real")
      ))
    }
  )
)

#' Gaussian Mutation (Real-Valued)
#' @export
MutationGaussian <- R6::R6Class("MutationGaussian",
  inherit = MutationOperator,
  public = list(
    mutate = function(genes, rate, sigma = 0.1, lower = -Inf, upper = Inf) {
      mask <- runif(length(genes)) < rate
      noise <- rnorm(sum(mask), mean = 0, sd = sigma)
      genes[mask] <- genes[mask] + noise
      # Clamp to bounds
      genes <- pmax(pmin(genes, upper), lower)
      return(genes)
    }
  )
)
