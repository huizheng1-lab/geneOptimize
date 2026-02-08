#' Plot Convergence Curve
#' 
#' Plots the convergence curve showing best fitness over generations.
#' 
#' @param x gene_opt_res object.
#' @param col Line color (default: "steelblue").
#' @param lwd Line width (default: 2).
#' @param add Add to existing plot (default: FALSE).
#' @param ... Additional arguments for plot().
#' @return NULL.
#' @export
#' @importFrom graphics plot lines grid
plot_convergence <- function(x, col = "steelblue", lwd = 2, add = FALSE, ...) {
  if (!inherits(x, "gene_opt_res")) {
    stop("x must be a gene_opt_res object")
  }
  
  generations <- seq_along(x$history)
  
  if (!add) {
    graphics::plot(generations, x$history, type = "l", col = col, lwd = lwd,
                   main = "Genetic Algorithm Convergence",
                   xlab = "Generation", ylab = "Best Fitness", ...)
    graphics::grid(col = "lightgray")
  } else {
    graphics::lines(generations, x$history, col = col, lwd = lwd, ...)
  }
  
  invisible(NULL)
}

#' Plot Fitness Statistics
#' 
#' Plots best, mean, and worst fitness over generations.
#' Requires history to contain all fitness values (not just best).
#' 
#' @param x gene_opt_res object.
#' @param col Color scheme (default: NULL for default colors).
#' @param lwd Line width (default: 2).
#' @param alpha Transparency for confidence band (default: 0.2).
#' @param ... Additional arguments for plot().
#' @return NULL.
#' @export
plot_fitness_stats <- function(x, col = NULL, lwd = 2, alpha = 0.2, ...) {
  if (!inherits(x, "gene_opt_res")) {
    stop("x must be a gene_opt_res object")
  }
  
  generations <- seq_along(x$history)
  
  # Default colors
  if (is.null(col)) {
    col_best <- "#2E7D32"    # Green - best
    col_mean <- "#1976D2"    # Blue - mean  
    col_worst <- "#D32F2F"   # Red - worst
  } else {
    col_best <- col[1]
    col_mean <- col[2]
    col_worst <- col[3]
  }
  
  # Try to get full fitness history from the result
  if (!is.null(x$mean_history) && !is.null(x$worst_history)) {
    # Full stats available
    graphics::plot(generations, x$history, type = "n", 
                   main = "Fitness Statistics Over Generations",
                   xlab = "Generation", ylab = "Fitness", ...)
    
    # Add confidence band
    graphics::polygon(c(generations, rev(generations)),
                      c(x$worst_history, rev(x$history)),
                      col = grDevices::rgb(0.1, 0.1, 0.1, alpha = alpha),
                      border = NA)
    
    graphics::lines(generations, x$worst_history, col = col_worst, lwd = 1, lty = 2)
    graphics::lines(generations, x$mean_history, col = col_mean, lwd = lwd)
    graphics::lines(generations, x$history, col = col_best, lwd = lwd)
    
    graphics::legend("bottomright", 
                     legend = c("Best", "Mean", "Worst"),
                     col = c(col_best, col_mean, col_worst),
                     lwd = c(lwd, lwd, 1),
                     lty = c(1, 1, 2))
  } else {
    # Only best fitness available - fallback to convergence
    warning("mean_history and worst_history not found. Using best fitness only.")
    plot_convergence(x, col = col_best, lwd = lwd, ...)
  }
  
  graphics::grid(col = "lightgray")
  invisible(NULL)
}

#' Plot Diversity Over Generations
#' 
#' Plots population diversity over generations (if diversity tracking is enabled).
#' 
#' @param x gene_opt_res object.
#' @param col Line color (default: "purple").
#' @param lwd Line width (default: 2).
#' @param type Plot type (default: "l").
#' @param ... Additional arguments for plot().
#' @return NULL.
#' @export
plot_diversity <- function(x, col = "purple", lwd = 2, type = "l", ...) {
  if (!inherits(x, "gene_opt_res")) {
    stop("x must be a gene_opt_res object")
  }
  
  if (is.null(x$diversity_history)) {
    stop("Diversity history not available. Enable diversity tracking in run_ga().")
  }
  
  generations <- seq_along(x$diversity_history)
  
  graphics::plot(generations, x$diversity_history, type = type, col = col, lwd = lwd,
                 main = "Population Diversity Over Generations",
                 xlab = "Generation", ylab = "Diversity (Std Dev)", ...)
  graphics::grid(col = "lightgray")
  
  invisible(NULL)
}

#' Diagnostic Plot for GA Results
#' 
#' Creates a comprehensive 4-panel diagnostic plot:
#' 1. Convergence curve
#' 2. Fitness distribution (boxplot per generation)
#' 3. Improvement rate
#' 4. Summary statistics
#' 
#' @param x gene_opt_res object.
#' @param mfrow Set custom mfrow (default: c(2, 2)).
#' @param ... Additional arguments.
#' @return NULL.
#' @export
plot_diagnostics <- function(x, mfrow = c(2, 2), ...) {
  if (!inherits(x, "gene_opt_res")) {
    stop("x must be a gene_opt_res object")
  }
  
  old_par <- graphics::par(mfrow = mfrow)
  on.exit(graphics::par(old_par))
  
  generations <- seq_along(x$history)
  
  # 1. Convergence
  plot_convergence(x, ...)
  
  # 2. Improvement rate
  improvement <- diff(x$history)
  positive_improvement <- pmax(0, improvement)
  graphics::plot(generations[-1], positive_improvement, type = "h", col = "green3",
                 main = "Improvement per Generation",
                 xlab = "Generation", ylab = "Improvement")
  graphics::grid(col = "lightgray")
  
  # 3. Rate of change
  rate_of_change <- abs(diff(x$history)) / pmax(1, x$history[-length(x$history)])
  graphics::plot(generations[-1], rate_of_change, type = "l", col = "orange",
                 main = "Relative Rate of Fitness Change",
                 xlab = "Generation", ylab = "Relative Change", log = "y")
  graphics::grid(col = "lightgray")
  
  # 4. Summary text
  graphics::plot.new()
  graphics::plot.window(c(0, 1), c(0, 1))
  text(0.5, 0.8, "GA Optimization Summary", cex = 1.5, font = 2)
  text(0.5, 0.6, paste("Best Fitness:", round(x$best_fitness, 4)), cex = 1.2)
  text(0.5, 0.45, paste("Generations:", length(x$history)), cex = 1.2)
  
  # Calculate improvement
  improvement_total <- x$history[length(x$history)] - x$history[1]
  text(0.5, 0.3, paste("Total Improvement:", round(improvement_total, 4)), cex = 1.2)
  
  # Convergence indicator
  if (length(x$history) > 10) {
    recent_improvement <- x$history[length(x$history)] - x$history[length(x$history) - 10]
    converged <- recent_improvement < 0.001 * abs(x$best_fitness)
    status <- if (converged) "CONVERGED" else "NOT CONVERGED"
    text(0.5, 0.15, paste("Status:", status), cex = 1.2, 
         col = if (converged) "green4" else "red")
  }
  
  invisible(NULL)
}

#' Compare Multiple GA Runs
#' 
#' Compares convergence curves from multiple runs.
#' 
#' @param ... gene_opt_res objects.
#' @param labels Labels for each run (default: paste("Run", seq_along(...))).
#' @param col Colors for each run.
#' @param lwd Line width (default: 2).
#' @param legend_pos Legend position (default: "bottomright").
#' @return NULL.
#' @export
plot_compare <- function(..., labels = NULL, col = NULL, lwd = 2, legend_pos = "bottomright") {
  results <- list(...)
  
  if (length(results) == 0) {
    stop("At least one gene_opt_res object required")
  }
  
  # Check all inputs
  for (i in seq_along(results)) {
    if (!inherits(results[[i]], "gene_opt_res")) {
      stop(sprintf("Argument %d is not a gene_opt_res object", i))
    }
  }
  
  n_runs <- length(results)
  
  # Default colors
  if (is.null(col)) {
    col <- grDevices::palette.colors(n_runs, palette = "Tableau")
  }
  
  # Default labels
  if (is.null(labels)) {
    labels <- paste("Run", seq_len(n_runs))
  }
  
  # Find y-axis range
  y_min <- min(sapply(results, function(r) min(r$history)))
  y_max <- max(sapply(results, function(r) max(r$history)))
  
  # Create first plot
  generations <- seq_along(results[[1]]$history)
  graphics::plot(generations, results[[1]]$history, type = "l", col = col[1], lwd = lwd,
                 main = "Comparison of GA Runs",
                 xlab = "Generation", ylab = "Best Fitness",
                 ylim = c(y_min, y_max))
  graphics::grid(col = "lightgray")
  
  # Add remaining lines
  for (i in 2:n_runs) {
    generations <- seq_along(results[[i]]$history)
    graphics::lines(generations, results[[i]]$history, col = col[i], lwd = lwd)
  }
  
  graphics::legend(legend_pos, legend = labels, col = col, lwd = lwd)
  
  invisible(NULL)
}

#' Summary Method for GA Results
#' 
#' Provides a comprehensive summary of GA optimization results.
#' 
#' @param object gene_opt_res object.
#' @param ... Additional arguments.
#' @return NULL.
#' @export
#' @importFrom stats sd median
summary.gene_opt_res <- function(object, ...) {
  cat("\n=== Genetic Algorithm Optimization Summary ===\n\n")
  
  cat("Best Fitness Value:", round(object$best_fitness, 6), "\n")
  cat("Best Chromosome Length:", length(object$best_chromosome), "\n")
  cat("Total Generations:", length(object$history), "\n\n")
  
  cat("Fitness Progression:\n")
  cat("  Initial Fitness:", round(object$history[1], 6), "\n")
  cat("  Final Fitness:", round(object$history[length(object$history)], 6), "\n")
  cat("  Total Improvement:", round(object$history[length(object$history)] - object$history[1], 6), "\n")
  cat("  Improvement %:", 
      round(100 * (object$history[length(object$history)] - object$history[1]) / 
              max(1, abs(object$history[1])), 2), "%\n\n")
  
  # Check for convergence
  if (length(object$history) > 10) {
    recent <- tail(object$history, 10)
    improvement_rate <- stats::sd(recent) / max(1, mean(recent))
    cat("Convergence Analysis:\n")
    cat("  Recent Stability (CV):", round(improvement_rate * 100, 2), "%\n")
    cat("  Status:", if (improvement_rate < 0.001) "LIKELY CONVERGED" else "STILL IMPROVING", "\n")
  }
  
  cat("\n")
  invisible(NULL)
}
