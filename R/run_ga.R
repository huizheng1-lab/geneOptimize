#' Functional interface for Genetic Algorithm
#'
#' @description A high-level wrapper to run the Genetic Algorithm.
#'
#' @param fitness_fn Fitness function that accepts a numeric vector (one
#'   chromosome) and returns a single numeric value.  Must be a **pure
#'   function** (no side effects, no global-state reads) when
#'   \code{parallel = TRUE}; see the \emph{Parallel Processing} section below.
#' @param n_genes Positive integer.  Number of genes (chromosome length).
#' @param pop_size Integer \eqn{\geq 2}.  Population size (default: 50).
#' @param generations Positive integer.  Number of generations (default: 100).
#' @param selection_fn R6 selection operator (default:
#'   \code{SelectionTournament$new()}).
#' @param crossover_fn R6 crossover operator (default:
#'   \code{CrossoverSinglePoint$new()}).
#' @param mutation_fn R6 mutation operator (default:
#'   \code{MutationSimple$new()}).
#' @param crossover_rate Numeric in \eqn{[0, 1]}.  Probability of crossover
#'   per pair (default: 0.8).
#' @param mutation_rate Numeric in \eqn{[0, 1]}.  Per-gene mutation
#'   probability (default: 0.01).
#' @param type Character.  Encoding type: \code{"binary"} (default) or
#'   \code{"real"}.
#' @param lower Numeric vector of lower bounds for real-valued genes.  A
#'   scalar is recycled to length \code{n_genes}.  \strong{Required} when
#'   \code{type = "real"}.
#' @param upper Numeric vector of upper bounds for real-valued genes.  A
#'   scalar is recycled to length \code{n_genes}.  \strong{Required} when
#'   \code{type = "real"}.  Every element must be strictly greater than the
#'   corresponding element of \code{lower}.
#' @param maximize Logical.  If \code{TRUE} (default), the GA maximizes
#'   \code{fitness_fn}.  Set to \code{FALSE} to minimize.
#' @param elitism_count Non-negative integer less than \code{pop_size}.
#'   Number of elite individuals copied unchanged to the next generation
#'   (default: 1).
#' @param verbose Logical.  Print per-generation progress (default: \code{TRUE}).
#' @param parallel Logical.  Parallelize fitness evaluation (default:
#'   \code{FALSE}).  See \emph{Parallel Processing} below.
#' @param cores Integer.  Number of cores for parallel evaluation (default: 1).
#' @param local_search_fn Optional function for Memetic Algorithm
#'   hybridization.  Receives a chromosome vector and returns an (optionally
#'   improved) chromosome vector.  Applied after mutation; frequency controlled
#'   by \code{local_search_every} and scope by \code{local_search_top_k}.
#' @param local_search_every Positive integer.  Apply local search every this
#'   many generations (default: 1 = every generation).
#' @param local_search_top_k Positive integer or \code{NULL}.  If set, local
#'   search is applied only to the top-\eqn{k} individuals (by current
#'   fitness) each eligible generation.  \code{NULL} (default) applies to all
#'   individuals.
#' @param track_diversity Logical.  Record mean pairwise population diversity
#'   each generation using \code{stats::dist()} — an O(n^2) operation.
#'   Default: \code{FALSE}.
#' @param ... Additional arguments forwarded to operator methods.
#'
#' @section Parallel Processing:
#' When \code{parallel = TRUE} and \code{cores > 1}, fitness evaluation is
#' distributed across workers using \code{parallel::mclapply()} on Unix-like
#' systems or \code{parallel::makeCluster()} + \code{parallel::parApply()} on
#' Windows.
#'
#' \strong{Requirements for parallel mode:}
#' \itemize{
#'   \item \code{fitness_fn} must be a \emph{pure function}: no global-state
#'     reads/writes, no side effects (file I/O, database writes, etc.).
#'   \item All objects needed by \code{fitness_fn} must be available in its
#'     closure or passed as arguments — worker processes do not share the
#'     parent session's environment.
#'   \item Functions that depend on a shared random stream will behave
#'     non-deterministically across workers.
#' }
#'
#' @return An object of class \code{gene_opt_res} with components:
#'   \describe{
#'     \item{\code{best_chromosome}}{Best chromosome found.}
#'     \item{\code{best_fitness}}{Fitness of the best chromosome.}
#'     \item{\code{history}}{Best fitness per generation.}
#'     \item{\code{mean_history}}{Mean fitness per generation.}
#'     \item{\code{worst_history}}{Worst fitness per generation.}
#'     \item{\code{diversity_history}}{Mean pairwise distance per generation
#'       (only present when \code{track_diversity = TRUE}).}
#'     \item{\code{call}}{The matched call.}
#'   }
#'
#' @export
run_ga <- function(fitness_fn,
                   n_genes,
                   pop_size            = 50,
                   generations         = 100,
                   selection_fn        = SelectionTournament$new(),
                   crossover_fn        = CrossoverSinglePoint$new(),
                   mutation_fn         = MutationSimple$new(),
                   crossover_rate      = 0.8,
                   mutation_rate       = 0.01,
                   type                = "binary",
                   lower               = NULL,
                   upper               = NULL,
                   maximize            = TRUE,
                   elitism_count       = 1,
                   verbose             = TRUE,
                   parallel            = FALSE,
                   cores               = 1,
                   local_search_fn     = NULL,
                   local_search_every  = 1,
                   local_search_top_k  = NULL,
                   track_diversity     = FALSE,
                   ...) {

  # -------------------------------------------------------------------------
  # Input Validation
  # -------------------------------------------------------------------------
  if (!is.function(fitness_fn)) {
    stop("'fitness_fn' must be a function.")
  }
  if (!is.numeric(n_genes) || length(n_genes) != 1 ||
      n_genes < 1 || n_genes != round(n_genes)) {
    stop("'n_genes' must be a positive integer.")
  }
  if (!is.numeric(pop_size) || length(pop_size) != 1 ||
      pop_size < 2 || pop_size != round(pop_size)) {
    stop("'pop_size' must be an integer >= 2.")
  }
  if (!is.numeric(generations) || length(generations) != 1 ||
      generations < 1 || generations != round(generations)) {
    stop("'generations' must be a positive integer.")
  }
  if (!is.numeric(crossover_rate) || length(crossover_rate) != 1 ||
      crossover_rate < 0 || crossover_rate > 1) {
    stop("'crossover_rate' must be in [0, 1].")
  }
  if (!is.numeric(mutation_rate) || length(mutation_rate) != 1 ||
      mutation_rate < 0 || mutation_rate > 1) {
    stop("'mutation_rate' must be in [0, 1].")
  }
  if (!type %in% c("binary", "real")) {
    stop("'type' must be \"binary\" or \"real\".")
  }
  if (!is.numeric(elitism_count) || length(elitism_count) != 1 ||
      elitism_count < 0 || elitism_count >= pop_size) {
    stop("'elitism_count' must be a non-negative integer less than 'pop_size'.")
  }
  if (!is.logical(maximize) || length(maximize) != 1) {
    stop("'maximize' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.numeric(local_search_every) || length(local_search_every) != 1 ||
      local_search_every < 1 ||
      local_search_every != round(local_search_every)) {
    stop("'local_search_every' must be a positive integer.")
  }
  if (!is.null(local_search_top_k)) {
    if (!is.numeric(local_search_top_k) || length(local_search_top_k) != 1 ||
        local_search_top_k < 1 || local_search_top_k > pop_size) {
      stop("'local_search_top_k' must be a positive integer <= 'pop_size', or NULL.")
    }
  }
  if (type == "real") {
    if (is.null(lower) || is.null(upper)) {
      stop("'lower' and 'upper' must be provided when type = \"real\".")
    }
    if (!is.numeric(lower) || !is.numeric(upper)) {
      stop("'lower' and 'upper' must be numeric.")
    }
    lower <- rep_len(lower, n_genes)
    upper <- rep_len(upper, n_genes)
    if (any(lower >= upper)) {
      stop("All elements of 'lower' must be strictly less than the ",
           "corresponding elements of 'upper'.")
    }
  }

  # -------------------------------------------------------------------------
  # Population Initialization
  # -------------------------------------------------------------------------
  if (type == "binary") {
    population <- matrix(
      sample(c(0L, 1L), pop_size * n_genes, replace = TRUE),
      nrow = pop_size
    )
  } else {
    population <- matrix(
      stats::runif(pop_size * n_genes,
                   rep_len(lower, pop_size * n_genes),
                   rep_len(upper, pop_size * n_genes)),
      nrow = pop_size
    )
  }

  best_history      <- numeric(generations)
  mean_history      <- numeric(generations)
  worst_history     <- numeric(generations)
  diversity_history <- numeric(generations)

  # Direction helpers
  best_idx_fn  <- if (maximize) which.max else which.min
  worst_idx_fn <- if (maximize) which.min else which.max
  order_decr   <- maximize   # descending order keeps best first when maximizing

  # -------------------------------------------------------------------------
  # Main GA Loop
  # -------------------------------------------------------------------------
  for (gen in seq_len(generations)) {

    # --- Fitness Evaluation ------------------------------------------------
    if (parallel && cores > 1) {
      if (.Platform$OS.type == "unix") {
        fitness_values <- unlist(
          parallel::mclapply(seq_len(nrow(population)),
                             function(i) fitness_fn(population[i, ]),
                             mc.cores = cores)
        )
      } else {
        cl <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        fitness_values <- parallel::parApply(cl, population, 1, fitness_fn)
      }
    } else {
      fitness_values <- apply(population, 1, fitness_fn)
    }

    # --- History Recording -------------------------------------------------
    best_idx             <- best_idx_fn(fitness_values)
    current_best_fitness <- fitness_values[best_idx]
    best_history[gen]    <- current_best_fitness
    mean_history[gen]    <- mean(fitness_values)
    worst_history[gen]   <- fitness_values[worst_idx_fn(fitness_values)]

    # --- Diversity Tracking (O(n^2), off by default) -----------------------
    # Uses stats::dist() for vectorised C-level computation instead of
    # the previous Vectorize()-based outer() loop.
    if (track_diversity) {
      if (type == "binary") {
        # Manhattan distance on a binary matrix equals Hamming distance:
        # the count of positions at which two chromosomes differ.
        div_mat <- as.matrix(stats::dist(population, method = "manhattan"))
      } else {
        div_mat <- as.matrix(stats::dist(population, method = "euclidean"))
      }
      diversity_history[gen] <- mean(div_mat[lower.tri(div_mat)])
    }

    if (verbose) {
      direction_lbl <- if (maximize) "Max" else "Min"
      cat(sprintf("Gen %d: Best (%s) = %f, Mean = %f\n",
                  gen, direction_lbl, current_best_fitness, mean_history[gen]))
    }

    # --- Elitism -----------------------------------------------------------
    order_idx <- order(fitness_values, decreasing = order_decr)
    elites    <- population[order_idx[seq_len(elitism_count)], , drop = FALSE]

    # --- Selection ---------------------------------------------------------
    new_pop <- matrix(0, nrow = pop_size, ncol = n_genes)
    for (i in seq_len(pop_size)) {
      new_pop[i, ] <- selection_fn$select(population, fitness_values, ...)
    }

    # --- Crossover ---------------------------------------------------------
    for (i in seq(1, pop_size - 1, by = 2)) {
      if (stats::runif(1) < crossover_rate) {
        children         <- crossover_fn$mate(new_pop[i, ], new_pop[i + 1, ])
        new_pop[i, ]     <- children[[1]]
        new_pop[i + 1, ] <- children[[2]]
      }
    }

    # --- Mutation ----------------------------------------------------------
    for (i in seq_len(pop_size)) {
      new_pop[i, ] <- mutation_fn$mutate(
        new_pop[i, ], type, mutation_rate,
        lower = lower, upper = upper, ...
      )
    }

    # --- Local Search (Memetic Hybridization) ------------------------------
    if (!is.null(local_search_fn) && (gen %% local_search_every == 0)) {
      if (is.null(local_search_top_k)) {
        ls_indices <- seq_len(pop_size)
      } else {
        # Re-evaluate mutated individuals to identify top-k for local search
        ls_fitness <- apply(new_pop, 1, fitness_fn)
        ls_indices <- order(ls_fitness, decreasing = order_decr)[
          seq_len(local_search_top_k)
        ]
      }
      for (i in ls_indices) {
        new_pop[i, ] <- local_search_fn(new_pop[i, ])
      }
    }

    # --- Replace population (elites always override first rows) ------------
    if (elitism_count > 0) {
      new_pop[seq_len(elitism_count), ] <- elites
    }
    population <- new_pop
  }

  # -------------------------------------------------------------------------
  # Final evaluation and result construction
  # -------------------------------------------------------------------------
  final_fitness  <- apply(population, 1, fitness_fn)
  best_final_idx <- best_idx_fn(final_fitness)

  res <- new_gene_opt_res(
    best_chromosome = population[best_final_idx, ],
    best_fitness    = final_fitness[best_final_idx],
    history         = best_history,
    call            = match.call()
  )

  res$mean_history  <- mean_history
  res$worst_history <- worst_history
  if (track_diversity) {
    res$diversity_history <- diversity_history
  }

  return(res)
}
