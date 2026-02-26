make_res <- function(n = 10) {
  new_gene_opt_res(
    best_chromosome = c(1L, 0L, 1L),
    best_fitness    = 0.9,
    history         = seq(0.5, 0.9, length.out = n),
    call            = quote(run_ga())
  )
}

test_that("plot_convergence runs without error", {
  expect_silent(plot_convergence(make_res()))
})

test_that("plot_convergence errors on wrong input type", {
  expect_error(plot_convergence(list()), "x must be a gene_opt_res object")
  expect_error(plot_convergence("text"), "x must be a gene_opt_res object")
})

test_that("plot_fitness_stats runs with full stats", {
  res                <- make_res()
  res$mean_history   <- seq(0.4, 0.85, length.out = 10)
  res$worst_history  <- seq(0.3, 0.8,  length.out = 10)
  expect_silent(plot_fitness_stats(res))
})

test_that("plot_fitness_stats falls back gracefully without mean/worst", {
  res <- make_res()
  expect_warning(plot_fitness_stats(res), "mean_history")
})

test_that("plot_diversity errors with actionable message when not tracked", {
  res <- make_res()
  expect_error(plot_diversity(res), "track_diversity = TRUE")
})

test_that("plot_diversity runs when diversity is tracked", {
  res                    <- make_res()
  res$diversity_history  <- seq(2.0, 0.5, length.out = 10)
  expect_silent(plot_diversity(res))
})

test_that("plot_diagnostics runs without error", {
  expect_silent(plot_diagnostics(make_res(n = 20)))
})

test_that("plot_diagnostics respects conv_threshold", {
  res <- make_res(n = 20)
  # Should not error with custom threshold
  expect_silent(plot_diagnostics(res, conv_threshold = 0.05))
})

test_that("plot_compare runs with two results", {
  r1 <- make_res()
  r2 <- make_res()
  r2$history <- seq(0.4, 0.85, length.out = 10)
  expect_silent(plot_compare(r1, r2, labels = c("Run 1", "Run 2")))
})

test_that("plot_compare errors with no arguments", {
  expect_error(plot_compare(), "At least one gene_opt_res object required")
})

test_that("plot_compare errors with wrong input type", {
  expect_error(plot_compare(list()), "not a gene_opt_res object")
})
