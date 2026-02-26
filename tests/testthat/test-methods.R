test_that("new_gene_opt_res creates correct structure", {
  res <- new_gene_opt_res(
    best_chromosome = c(1L, 0L, 1L),
    best_fitness    = 0.9,
    history         = c(0.5, 0.7, 0.9),
    call            = quote(run_ga())
  )
  expect_s3_class(res, "gene_opt_res")
  expect_equal(res$best_fitness, 0.9)
  expect_equal(res$best_chromosome, c(1L, 0L, 1L))
  expect_length(res$history, 3)
})

test_that("print.gene_opt_res outputs expected header", {
  res <- new_gene_opt_res(c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L),
                          0.8, seq(0.4, 0.8, length.out = 10), quote(run_ga()))
  expect_output(print(res), "Genetic Algorithm Optimization Result")
  expect_output(print(res), "Best Fitness found")
})

test_that("print.gene_opt_res truncates long chromosomes", {
  res <- new_gene_opt_res(rep(1L, 50), 0.9,
                          seq(0.5, 0.9, length.out = 5), quote(run_ga()))
  expect_output(print(res), "length 50")
})

test_that("summary.gene_opt_res outputs required sections", {
  res <- new_gene_opt_res(c(1L, 0L, 1L), 0.9,
                          seq(0.5, 0.9, length.out = 20), quote(run_ga()))
  expect_output(summary(res), "Genetic Algorithm Optimization Summary")
  expect_output(summary(res), "Best Fitness")
  expect_output(summary(res), "Total Generations")
  expect_output(summary(res), "Convergence")
})

test_that("plot.gene_opt_res runs without error", {
  res <- new_gene_opt_res(c(1L, 0L, 1L), 0.9,
                          seq(0.5, 0.9, length.out = 10), quote(run_ga()))
  expect_silent(plot(res))
})
