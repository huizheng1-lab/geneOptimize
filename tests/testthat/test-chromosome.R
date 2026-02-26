test_that("BinaryChromosome initializes correctly", {
  chrom <- BinaryChromosome$new(c(1L, 0L, 1L, 1L, 0L))
  expect_equal(chrom$genes, c(1L, 0L, 1L, 1L, 0L))
  expect_equal(chrom$type, "binary")
  expect_equal(chrom$fitness, -Inf)
})

test_that("BinaryChromosome mutate flips bits and stays binary", {
  set.seed(42)
  chrom <- BinaryChromosome$new(rep(1L, 100))
  chrom$mutate(mutation_rate = 0.5)
  expect_true(any(chrom$genes == 0))
  expect_true(all(chrom$genes %in% c(0, 1)))
})

test_that("RealChromosome initializes correctly", {
  genes <- c(0.1, 0.5, 0.9)
  chrom <- RealChromosome$new(genes)
  expect_equal(chrom$genes, genes)
  expect_equal(chrom$type, "real")
  expect_equal(chrom$fitness, -Inf)
})

test_that("RealChromosome mutate respects bounds", {
  set.seed(42)
  chrom <- RealChromosome$new(rep(0.5, 50))
  chrom$mutate(mutation_rate = 1.0, lower = 0, upper = 1)
  expect_true(all(chrom$genes >= 0 & chrom$genes <= 1))
})

test_that("RealChromosome mutate resets fitness to -Inf", {
  chrom <- RealChromosome$new(rep(0.5, 5))
  chrom$fitness <- 0.9
  chrom$mutate(mutation_rate = 0.01, lower = 0, upper = 1)
  expect_equal(chrom$fitness, -Inf)
})

test_that("Chromosome copy creates independent deep clone", {
  chrom  <- BinaryChromosome$new(c(1L, 0L, 1L))
  chrom2 <- chrom$copy()
  chrom2$genes[1] <- 0L
  expect_equal(chrom$genes[1], 1L)
})
