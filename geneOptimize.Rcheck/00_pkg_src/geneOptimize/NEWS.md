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
