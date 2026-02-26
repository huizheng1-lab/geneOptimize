test_that("SelectionTournament returns a row from the population", {
  set.seed(42)
  pop     <- matrix(c(1, 0, 1, 0, 1,
                      0, 1, 0, 1, 0,
                      1, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
  fitness <- c(0.5, 0.8, 0.3)
  sel     <- SelectionTournament$new(k = 2)
  result  <- sel$select(pop, fitness)
  expect_length(result, 5)
  expect_true(any(apply(pop, 1, function(row) all(row == result))))
})

test_that("SelectionTournament k=1 always returns a valid individual", {
  set.seed(42)
  pop     <- matrix(runif(15), nrow = 3)
  fitness <- c(0.5, 0.8, 0.3)
  sel     <- SelectionTournament$new(k = 1)
  result  <- sel$select(pop, fitness)
  expect_length(result, 5)
})

test_that("SelectionRoulette returns a row from the population", {
  set.seed(42)
  pop     <- matrix(runif(20), nrow = 4)
  fitness <- c(0.2, 0.5, 0.1, 0.8)
  sel     <- SelectionRoulette$new()
  result  <- sel$select(pop, fitness)
  expect_length(result, 5)
})

test_that("SelectionRoulette handles all-negative fitness", {
  set.seed(42)
  pop     <- matrix(runif(15), nrow = 3)
  fitness <- c(-2.0, -1.0, -3.0)
  sel     <- SelectionRoulette$new()
  result  <- sel$select(pop, fitness)
  expect_length(result, 5)
})

test_that("SelectionRoulette handles equal fitness values (uniform selection)", {
  set.seed(42)
  pop     <- matrix(runif(15), nrow = 3)
  fitness <- c(1.0, 1.0, 1.0)
  sel     <- SelectionRoulette$new()
  result  <- sel$select(pop, fitness)
  expect_length(result, 5)
})

# ---------- Crossover tests ----------

test_that("CrossoverSinglePoint produces two children of correct length", {
  set.seed(42)
  cx       <- CrossoverSinglePoint$new()
  p1       <- c(1, 0, 1, 0, 1)
  p2       <- c(0, 1, 0, 1, 0)
  children <- cx$mate(p1, p2)
  expect_length(children, 2)
  expect_length(children[[1]], 5)
  expect_length(children[[2]], 5)
})

test_that("CrossoverSinglePoint genes come from parents", {
  set.seed(42)
  cx       <- CrossoverSinglePoint$new()
  p1       <- c(1, 1, 1, 1, 1)
  p2       <- c(0, 0, 0, 0, 0)
  children <- cx$mate(p1, p2)
  expect_true(all(children[[1]] %in% c(0, 1)))
  expect_true(all(children[[2]] %in% c(0, 1)))
})

test_that("CrossoverUniform children genes are from parents", {
  set.seed(42)
  cx       <- CrossoverUniform$new()
  p1       <- c(1, 1, 1, 1, 1)
  p2       <- c(0, 0, 0, 0, 0)
  children <- cx$mate(p1, p2)
  expect_true(all(children[[1]] %in% c(0, 1)))
  expect_true(all(children[[2]] %in% c(0, 1)))
  # Genes are complementary when parents are all-1 and all-0
  expect_true(all(children[[1]] + children[[2]] == 1))
})

test_that("CrossoverArithmetic with alpha=0.5 produces midpoints", {
  cx       <- CrossoverArithmetic$new(alpha = 0.5)
  p1       <- c(1.0, 1.0, 1.0)
  p2       <- c(0.0, 0.0, 0.0)
  children <- cx$mate(p1, p2)
  expect_equal(children[[1]], c(0.5, 0.5, 0.5))
  expect_equal(children[[2]], c(0.5, 0.5, 0.5))
})

test_that("CrossoverArithmetic with alpha=0 swaps parents", {
  cx       <- CrossoverArithmetic$new(alpha = 0)
  p1       <- c(1.0, 2.0)
  p2       <- c(3.0, 4.0)
  children <- cx$mate(p1, p2)
  expect_equal(children[[1]], p2)
  expect_equal(children[[2]], p1)
})

# ---------- Mutation tests ----------

test_that("MutationSimple binary bit-flip produces valid binary output", {
  set.seed(42)
  mut    <- MutationSimple$new()
  genes  <- rep(1L, 100)
  result <- mut$mutate(genes, "binary", rate = 0.5)
  expect_true(all(result %in% c(0, 1)))
  expect_true(sum(result == 0) > 0)
})

test_that("MutationSimple binary rate=0 makes no change", {
  mut    <- MutationSimple$new()
  genes  <- c(1L, 0L, 1L, 1L)
  result <- mut$mutate(genes, "binary", rate = 0)
  expect_equal(result, genes)
})

test_that("MutationSimple real respects bounds", {
  set.seed(42)
  mut    <- MutationSimple$new()
  genes  <- rep(0.5, 50)
  result <- mut$mutate(genes, "real", rate = 1.0, lower = 0, upper = 1)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("MutationGaussian respects bounds even with large sigma", {
  set.seed(42)
  mut    <- MutationGaussian$new(sigma = 100)
  genes  <- rep(0.0, 50)
  result <- mut$mutate(genes, "real", rate = 1.0, lower = -1, upper = 1)
  expect_true(all(result >= -1 & result <= 1))
})

test_that("MutationGaussian sigma stored as field", {
  mut <- MutationGaussian$new(sigma = 0.25)
  expect_equal(mut$sigma, 0.25)
})
