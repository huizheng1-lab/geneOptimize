#' Real-Valued Function Optimization
#' @description This example demonstrates using \code{geneOptimize} to optimize 
#' a real-valued function (e.g., the Ackley function) using \code{RealChromosome} 
#' and Gaussian mutation.
#' @name example_real_optimization
#' @examples
#' \dontrun{
#' library(geneOptimize)
#' 
#' # 1. Define Ackley Function (Goal: Minimize, so return -Ackley)
#' ackley <- function(x) {
#'   d <- length(x)
#'   a <- 20; b <- 0.2; c <- 2*pi
#'   sum1 <- sum(x^2)
#'   sum2 <- sum(cos(c*x))
#'   term1 <- -a * exp(-b * sqrt(sum1/d))
#'   term2 <- -exp(sum2/d)
#'   return(-(term1 + term2 + a + exp(1)))
#' }
#' 
#' # 2. Execute GA for Real-Valued Optimization
#' results <- run_ga(
#'   fitness_fn = ackley,
#'   n_genes = 2,
#'   pop_size = 50,
#'   generations = 100,
#'   type = "real",
#'   lower = rep(-5, 2),
#'   upper = rep(5, 2),
#'   mutation_fn = MutationGaussian$new(),
#'   crossover_fn = CrossoverArithmetic$new(alpha = 0.5)
#' )
#' 
#' # 3. Review Results
#' print(results)
#' plot(results)
#' }
NULL

#' Memetic Algorithm for Local Refinement
#' @description This example demonstrates the hybrid "Memetic" approach 
#' described in the manuscript, where a local search step is added to the 
#' evolutionary loop.
#' @name example_memetic_algorithm
#' @examples
#' \dontrun{
#' library(geneOptimize)
#' 
#' # Define a simple fitness function
#' fitness_fn <- function(x) -sum((x - 0.5)^2)
#' 
#' # Define a local search (hill climbing) operator
#' local_search <- function(chromosome) {
#'   # Simple perturbation logic for local refinement
#'   refined <- chromosome + rnorm(length(chromosome), 0, 0.01)
#'   if (fitness_fn(refined) > fitness_fn(chromosome)) {
#'     return(refined)
#'   }
#'   return(chromosome)
#' }
#' 
#' # Run GA with local search hybridization
#' results <- run_ga(
#'   fitness_fn = fitness_fn,
#'   n_genes = 5,
#'   pop_size = 30,
#'   generations = 20,
#'   type = "real",
#'   lower = rep(0, 5),
#'   upper = rep(1, 5),
#'   local_search_fn = local_search # Injecting local refinement
#' )
#' 
#' print(results)
#' }
NULL
