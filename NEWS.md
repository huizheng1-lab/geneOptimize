# geneOptimize 1.2.0

## New Features

- `run_ga()`: Added `cache` parameter (default `FALSE`).  When `TRUE`, fitness
  evaluations are memoized using an in-process hash-table (R environment).  Any
  chromosome that has already been evaluated is not re-evaluated; its cached
  value is returned immediately.  The result object gains two new fields:
  `cache_hits` (number of evaluations served from cache) and `cache_size`
  (number of unique chromosomes stored).  Using `cache = TRUE` together with
  `parallel = TRUE` / `cores > 1` raises an informative error, as worker
  processes do not share the cache environment.

# geneOptimize 1.1.0

## New Features

- `run_ga()`: Added `maximize` parameter (default `TRUE`); set `FALSE` for
  minimization problems without negating the fitness function.
- `run_ga()`: `lower` and `upper` are now explicit named parameters (previously
  required via `...`), with scalar recycling and cross-validation.
- `run_ga()`: Added `local_search_every` — apply memetic local search every _n_
  generations instead of every generation.
- `run_ga()`: Added `local_search_top_k` — restrict memetic local search to the
  top-_k_ individuals, reducing computational cost.
- `plot_diagnostics()`: Added `conv_threshold` parameter to make the convergence
  indicator threshold configurable (default: 0.001 = 0.1%).
- `Population$initialize()`: Fully implemented; creates `BinaryChromosome` or
  `RealChromosome` objects, accepts `lower`/`upper` for real-valued populations.
- `Population$evaluate()` / `Population$get_best()`: Fully implemented.
- `GeneticAlgorithm$run()`: Fully implemented (delegates to `run_ga()`).
- Added comprehensive `testthat` test suite (60+ tests across 6 files):
  `test-chromosome.R`, `test-operators.R`, `test-run_ga.R`, `test-methods.R`,
  `test-plotting.R`, `test-population.R`.

## Improvements

- **Diversity tracking** now uses `stats::dist()` (vectorized C-level
  computation) instead of the previous `Vectorize(outer())` loop, providing
  substantial speedup for large populations.
- `SelectionRoulette`: Replaced magic constant `0.01` with documented `1e-6`;
  now correctly handles all-equal fitness values (uniform random selection).
- `MutationGaussian`: Fixed method signature to match `MutationSimple`
  (`mutate(genes, type, rate, lower, upper, ...)`). `sigma` is now a class
  field set at construction time (`MutationGaussian$new(sigma = 0.1)`).
- `plot_diversity()`: Error message now includes the exact corrective action
  (`run_ga(..., track_diversity = TRUE)`).
- `run_ga()`: Comprehensive input validation for all parameters with
  informative error messages.
- `run_ga()`: Parallel-mode documentation lists explicit requirements for pure
  fitness functions.
- Added `grDevices` to `Imports` and `pROC` to `Suggests`.
- Updated vignette/paper source (`vignettes/geneOptimize_paper.Rmd`) to
  document all new and corrected functionality.

## Bug Fixes

- Fixed `MutationGaussian$mutate()` parameter mapping (previously `type` was
  silently mapped to `rate`, and `mutation_rate` to `sigma`, causing wrong
  mutation behaviour when using `MutationGaussian` with `run_ga()`).
- Fixed `plot_diagnostics()` rate-of-change denominator to use
  `abs(history[-n])` instead of bare `history[-n]`, avoiding artefacts when
  fitness values are near zero.
- `Population` and `GeneticAlgorithm` stubs are now fully functional.

# geneOptimize (development version)

## New Features

- Added comprehensive plotting functions:
  - `plot_convergence()`: Convergence curve visualization
  - `plot_fitness_stats()`: Best, mean, and worst fitness over generations
  - `plot_diversity()`: Population diversity tracking
  - `plot_diagnostics()`: 4-panel comprehensive diagnostic plot
  - `plot_compare()`: Compare multiple GA runs
  - `summary.gene_opt_res()`: Detailed summary statistics

- Enhanced `run_ga()` to track:
  - Mean fitness history
  - Worst fitness history
  - Population diversity (when `track_diversity = TRUE`)

## Improvements

- Updated DESCRIPTION with proper Authors@R field
- Added knitr and rmarkdown to Suggests for vignette building
- Improved documentation with better examples

## Bug Fixes

- Fixed NAMESPACE exports for new plotting functions
- Added proper imports for graphics functions

# geneOptimize 1.0.0

- Initial CRAN release
- Core genetic algorithm framework with R6 classes
- Support for binary and real-valued encodings
- Modular operators (selection, crossover, mutation)
- Parallel processing via future package
- Memetic algorithm support with local search
- Basic S3 methods (print, plot) for results
