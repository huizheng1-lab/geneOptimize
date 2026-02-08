#' Constructor for GA result object
#' @param best_chromosome Best chromosome found.
#' @param best_fitness Best fitness value.
#' @param history Fitness history.
#' @param call Original function call.
#' @return An object of class \code{gene_opt_res}.
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
print.gene_opt_res <- function(x, ...) {
  cat("\n=== Genetic Algorithm Optimization Result ===\n")
  cat("Call: ")
  print(x$call)
  cat("\nBest Fitness found: ", x$best_fitness, "\n")
  cat("Best Chromosome: ", head(x$best_chromosome, 10))
  if (length(x$best_chromosome) > 10) cat(" ... (length", length(x$best_chromosome), ")")
  cat("\nGenerations run: ", length(x$history), "\n")
  invisible(x)
}

#' @export
plot.gene_opt_res <- function(x, ...) {
  graphics::plot(x$history, type = "l", col = "blue", lwd = 2,
                 main = "Genetic Algorithm Convergence",
                 xlab = "Generation", ylab = "Best Fitness")
  graphics::grid()
}
