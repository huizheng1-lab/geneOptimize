test_that("Population initializes with correct size (binary)", {
  pop <- Population$new(size = 10, n_genes = 5, type = "binary")
  expect_equal(pop$size, 10)
  expect_length(pop$individuals, 10)
  expect_equal(pop$individuals[[1]]$type, "binary")
  expect_length(pop$individuals[[1]]$genes, 5)
  expect_true(all(pop$individuals[[1]]$genes %in% c(0L, 1L)))
})

test_that("Population initializes with correct size (real)", {
  pop <- Population$new(size = 5, n_genes = 3, type = "real",
                        lower = -1, upper = 1)
  expect_equal(pop$size, 5)
  expect_length(pop$individuals, 5)
  expect_equal(pop$individuals[[1]]$type, "real")
  expect_true(all(pop$individuals[[1]]$genes >= -1 &
                    pop$individuals[[1]]$genes <= 1))
})

test_that("Population evaluate sets fitness values", {
  pop <- Population$new(size = 5, n_genes = 4, type = "binary")
  pop$evaluate(function(x) sum(x))
  fitnesses <- sapply(pop$individuals, function(ind) ind$fitness)
  expect_true(all(is.finite(fitnesses)))
  expect_true(all(fitnesses >= 0 & fitnesses <= 4))
})

test_that("Population get_best returns highest-fitness individual", {
  pop <- Population$new(size = 5, n_genes = 4, type = "binary")
  pop$evaluate(function(x) sum(x))
  best <- pop$get_best()
  fitnesses <- sapply(pop$individuals, function(ind) ind$fitness)
  expect_equal(best$fitness, max(fitnesses))
})
