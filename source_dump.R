### Chromosome ###
<Chromosome> object generator
  Public:
    genes: NULL
    fitness: -Inf
    type: NULL
    initialize: function (genes, type = "binary") 
    copy: function () 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### crossover_single_point ###
function (parent1, parent2) 
{
    n_genes <- length(parent1)
    point <- sample(1:(n_genes - 1), 1)
    child1 <- c(parent1[1:point], parent2[(point + 1):n_genes])
    child2 <- c(parent2[1:point], parent1[(point + 1):n_genes])
    return(list(child1, child2))
}
<bytecode: 0x7fa2c2e488f0>
<environment: namespace:geneOptimize>
### crossover_uniform ###
function (parent1, parent2) 
{
    n_genes <- length(parent1)
    mask <- sample(c(0, 1), n_genes, replace = TRUE)
    child1 <- ifelse(mask == 1, parent1, parent2)
    child2 <- ifelse(mask == 1, parent2, parent1)
    return(list(child1, child2))
}
<bytecode: 0x7fa2c2e52618>
<environment: namespace:geneOptimize>
### CrossoverOperator ###
<CrossoverOperator> object generator
  Public:
    mate: function (p1, p2) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### CrossoverSinglePoint ###
<CrossoverSinglePoint> object generator
  Inherits from: <CrossoverOperator>
  Public:
    mate: function (p1, p2) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### CrossoverUniform ###
<CrossoverUniform> object generator
  Inherits from: <CrossoverOperator>
  Public:
    mate: function (p1, p2) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### GeneticAlgorithm ###
<GeneticAlgorithm> object generator
  Public:
    pop: NULL
    generations: NULL
    crossover_rate: NULL
    mutation_rate: NULL
    fitness_fn: NULL
    history: NULL
    operators: list
    initialize: function (pop, fitness_fn, generations = 100, crossover_rate = 0.8, 
    run: function (verbose = TRUE, parallel = FALSE) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### mutation_binary ###
function (chromosome, mutation_rate, ...) 
{
    n_genes <- length(chromosome)
    mask <- stats::runif(n_genes) < mutation_rate
    chromosome[mask] <- 1 - chromosome[mask]
    return(chromosome)
}
<bytecode: 0x7fa2c6131cb0>
<environment: namespace:geneOptimize>
### mutation_real ###
function (chromosome, mutation_rate, lower, upper, ...) 
{
    n_genes <- length(chromosome)
    mask <- stats::runif(n_genes) < mutation_rate
    noise <- stats::rnorm(sum(mask), mean = 0, sd = (upper - 
        lower)/10)
    chromosome[mask] <- chromosome[mask] + noise
    return(pmax(pmin(chromosome, upper), lower))
}
<bytecode: 0x7fa2c6138730>
<environment: namespace:geneOptimize>
### MutationOperator ###
<MutationOperator> object generator
  Public:
    mutate: function (genes, type, rate) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### MutationSimple ###
<MutationSimple> object generator
  Inherits from: <MutationOperator>
  Public:
    mutate: function (genes, type, rate) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### new_gene_opt_res ###
function (best_chromosome, best_fitness, history, call) 
{
    structure(list(best_chromosome = best_chromosome, best_fitness = best_fitness, 
        history = history, call = call), class = "gene_opt_res")
}
<bytecode: 0x7fa2c2f37df8>
<environment: namespace:geneOptimize>
### Population ###
<Population> object generator
  Public:
    individuals: NULL
    size: NULL
    initialize: function (size, n_genes, type = "binary", ...) 
    evaluate: function (fitness_fn, parallel = FALSE) 
    get_best: function () 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### run_ga ###
function (fitness_fn, n_genes, pop_size = 50, generations = 100, 
    selection_fn = selection_tournament, crossover_fn = crossover_single_point, 
    mutation_fn = mutation_binary, crossover_rate = 0.8, mutation_rate = 0.01, 
    type = "binary", elitism_count = 1, verbose = TRUE, parallel = FALSE, 
    cores = 1, ...) 
{
    if (type == "binary") {
        population <- matrix(sample(c(0, 1), pop_size * n_genes, 
            replace = TRUE), nrow = pop_size)
    }
    else {
        args <- list(...)
        population <- matrix(stats::runif(pop_size * n_genes, 
            args$lower, args$upper), nrow = pop_size)
    }
    best_history <- numeric(generations)
    for (gen in 1:generations) {
        if (parallel && cores > 1) {
            if (.Platform$OS.type == "unix") {
                fitness_values <- unlist(parallel::mclapply(seq_len(nrow(population)), 
                  function(i) fitness_fn(population[i, ]), mc.cores = cores))
            }
            else {
                cl <- parallel::makeCluster(cores)
                on.exit(parallel::stopCluster(cl))
                fitness_values <- parallel::parApply(cl, population, 
                  1, fitness_fn)
            }
        }
        else {
            fitness_values <- apply(population, 1, fitness_fn)
        }
        best_idx <- which.max(fitness_values)
        current_best_fitness <- fitness_values[best_idx]
        best_history[gen] <- current_best_fitness
        order_idx <- order(fitness_values, decreasing = TRUE)
        elites <- population[order_idx[1:elitism_count], , drop = FALSE]
        if (verbose) {
            cat(sprintf("Gen %d: Best = %f\n", gen, current_best_fitness))
        }
        new_pop <- matrix(0, nrow = pop_size, ncol = n_genes)
        for (i in 1:pop_size) {
            new_pop[i, ] <- selection_fn(population, fitness_values, 
                ...)
        }
        for (i in seq(1, pop_size - 1, by = 2)) {
            if (stats::runif(1) < crossover_rate) {
                children <- crossover_fn(new_pop[i, ], new_pop[i + 
                  1, ])
                new_pop[i, ] <- children[[1]]
                new_pop[i + 1, ] <- children[[2]]
            }
        }
        for (i in 1:pop_size) {
            new_pop[i, ] <- mutation_fn(new_pop[i, ], mutation_rate, 
                ...)
        }
        new_pop[1:elitism_count, ] <- elites
        population <- new_pop
    }
    final_fitness <- apply(population, 1, fitness_fn)
    best_final_idx <- which.max(final_fitness)
    res <- new_gene_opt_res(best_chromosome = population[best_final_idx, 
        ], best_fitness = final_fitness[best_final_idx], history = best_history, 
        call = match.call())
    return(res)
}
<bytecode: 0x7fa2c2fd5500>
<environment: namespace:geneOptimize>
### selection_roulette ###
function (population, fitness_values, ...) 
{
    pop_size <- nrow(population)
    min_fit <- min(fitness_values)
    adj_fitness <- if (min_fit < 0) 
        fitness_values - min_fit + 0.01
    else fitness_values
    probs <- adj_fitness/sum(adj_fitness)
    idx <- sample(1:pop_size, 1, prob = probs)
    return(population[idx, ])
}
<bytecode: 0x7fa2c2fe0a48>
<environment: namespace:geneOptimize>
### selection_tournament ###
function (population, fitness_values, k = 3, ...) 
{
    pop_size <- nrow(population)
    indices <- sample(1:pop_size, k)
    winner <- indices[which.max(fitness_values[indices])]
    return(population[winner, ])
}
<bytecode: 0x7fa2c2fe7030>
<environment: namespace:geneOptimize>
### SelectionOperator ###
<SelectionOperator> object generator
  Public:
    select: function (pop, n) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
### SelectionTournament ###
<SelectionTournament> object generator
  Inherits from: <SelectionOperator>
  Public:
    k: 2
    initialize: function (k = 2) 
    select: function (pop, n) 
    clone: function (deep = FALSE) 
  Parent env: <environment: namespace:geneOptimize>
  Locked objects: TRUE
  Locked class: FALSE
  Portable: TRUE
