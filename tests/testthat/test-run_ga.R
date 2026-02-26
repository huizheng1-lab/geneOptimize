test_that("run_ga validates fitness_fn", {
  expect_error(run_ga(42, n_genes = 5), "'fitness_fn' must be a function")
})

test_that("run_ga validates n_genes", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = -1),  "'n_genes' must be a positive integer")
  expect_error(run_ga(fn, n_genes = 1.5), "'n_genes' must be a positive integer")
  expect_error(run_ga(fn, n_genes = 0),   "'n_genes' must be a positive integer")
})

test_that("run_ga validates pop_size", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, pop_size = 1), "'pop_size' must be an integer >= 2")
  expect_error(run_ga(fn, n_genes = 5, pop_size = -5), "'pop_size' must be an integer >= 2")
})

test_that("run_ga validates generations", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, generations = 0), "'generations' must be a positive integer")
})

test_that("run_ga validates crossover_rate", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, crossover_rate = 1.5), "'crossover_rate' must be in")
  expect_error(run_ga(fn, n_genes = 5, crossover_rate = -0.1), "'crossover_rate' must be in")
})

test_that("run_ga validates mutation_rate", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, mutation_rate = 2.0), "'mutation_rate' must be in")
})

test_that("run_ga validates type", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, type = "invalid"), "'type' must be")
})

test_that("run_ga requires lower/upper when type='real'", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, type = "real"), "'lower' and 'upper' must be provided")
  expect_error(run_ga(fn, n_genes = 5, type = "real", lower = 0), "'lower' and 'upper' must be provided")
})

test_that("run_ga validates lower < upper", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 2, type = "real", lower = 1, upper = 0),
               "strictly less than")
  expect_error(run_ga(fn, n_genes = 2, type = "real", lower = 1, upper = 1),
               "strictly less than")
})

test_that("run_ga validates maximize is logical", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, maximize = "yes"), "'maximize' must be a single logical value")
})

test_that("run_ga validates local_search_every", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, local_search_fn = fn, local_search_every = 0),
               "'local_search_every' must be a positive integer")
})

test_that("run_ga validates local_search_top_k", {
  fn <- function(x) sum(x)
  expect_error(run_ga(fn, n_genes = 5, pop_size = 10,
                      local_search_fn = fn, local_search_top_k = 20),
               "'local_search_top_k' must be")
})

# ---------- Functional tests ----------

test_that("run_ga returns gene_opt_res for binary type", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 10, pop_size = 10,
                generations = 5, verbose = FALSE)
  expect_s3_class(res, "gene_opt_res")
  expect_length(res$history, 5)
  expect_length(res$best_chromosome, 10)
  expect_true(all(res$best_chromosome %in% c(0, 1)))
})

test_that("run_ga returns gene_opt_res for real type", {
  set.seed(42)
  res <- run_ga(function(x) -sum(x^2), n_genes = 3, pop_size = 10,
                generations = 5, type = "real",
                lower = rep(-5, 3), upper = rep(5, 3), verbose = FALSE)
  expect_s3_class(res, "gene_opt_res")
  expect_length(res$best_chromosome, 3)
  expect_true(all(res$best_chromosome >= -5 & res$best_chromosome <= 5))
})

test_that("run_ga lower/upper recycles scalar bounds", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 5, pop_size = 8,
                generations = 3, type = "real",
                lower = -1, upper = 1, verbose = FALSE)
  expect_true(all(res$best_chromosome >= -1 & res$best_chromosome <= 1))
})

test_that("run_ga with maximize=FALSE finds minimum", {
  set.seed(42)
  # Minimize sum-of-squares; optimum at (0, 0)
  res <- run_ga(function(x) sum(x^2), n_genes = 2, pop_size = 30,
                generations = 50, type = "real",
                lower = rep(-5, 2), upper = rep(5, 2),
                maximize = FALSE, verbose = FALSE)
  expect_true(res$best_fitness < 1.0)
})

test_that("run_ga elitism means best fitness is non-decreasing (maximize)", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 10, pop_size = 20,
                generations = 15, elitism_count = 1, verbose = FALSE)
  expect_true(all(diff(res$history) >= -1e-10))
})

test_that("run_ga tracks mean_history and worst_history", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 5, pop_size = 10,
                generations = 5, verbose = FALSE)
  expect_length(res$mean_history, 5)
  expect_length(res$worst_history, 5)
})

test_that("run_ga diversity_history is NULL when not requested", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 5, pop_size = 10,
                generations = 3, verbose = FALSE)
  expect_null(res$diversity_history)
})

test_that("run_ga tracks diversity when requested (binary)", {
  set.seed(42)
  res <- run_ga(function(x) sum(x), n_genes = 5, pop_size = 8,
                generations = 4, track_diversity = TRUE, verbose = FALSE)
  expect_false(is.null(res$diversity_history))
  expect_length(res$diversity_history, 4)
  expect_true(all(res$diversity_history >= 0))
})

test_that("run_ga tracks diversity when requested (real)", {
  set.seed(42)
  res <- run_ga(function(x) -sum(x^2), n_genes = 2, pop_size = 8,
                generations = 4, type = "real", lower = -1, upper = 1,
                track_diversity = TRUE, verbose = FALSE)
  expect_false(is.null(res$diversity_history))
})

test_that("run_ga local_search_every controls call frequency", {
  call_count <- 0L
  local_fn   <- function(x) { call_count <<- call_count + 1L; x }
  set.seed(42)
  run_ga(function(x) sum(x), n_genes = 5, pop_size = 5, generations = 10,
         local_search_fn = local_fn, local_search_every = 2, verbose = FALSE)
  # Generations 2,4,6,8,10 → 5 gens × 5 individuals = 25 calls
  expect_equal(call_count, 25L)
})

test_that("run_ga local_search_top_k limits calls per generation", {
  call_count <- 0L
  local_fn   <- function(x) { call_count <<- call_count + 1L; x }
  set.seed(42)
  run_ga(function(x) sum(x), n_genes = 5, pop_size = 10, generations = 4,
         local_search_fn = local_fn, local_search_top_k = 3, verbose = FALSE)
  # 4 gens × 3 top-k individuals = 12 calls
  expect_equal(call_count, 12L)
})
