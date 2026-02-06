#' Chromosome R6 Class
#' @description Abstract container for the gene vector and associated fitness value.
#' @importFrom R6 R6Class
#' @export
Chromosome <- R6::R6Class("Chromosome",
  public = list(
    #' @field genes Vector of genes
    genes = NULL,
    #' @field fitness Numeric fitness value
    fitness = -Inf,
    #' @field type Character "binary" or "real"
    type = NULL,

    #' @description Initialize a new Chromosome
    #' @param genes Initial gene vector
    #' @param type "binary" or "real"
    initialize = function(genes, type = "binary") {
      self$genes <- genes
      self$type <- type
    },

    #' @description Create a copy of the chromosome
    copy = function() {
      new_chrom <- Chromosome$new(self$genes, self$type)
      new_chrom$fitness <- self$fitness
      return(new_chrom)
    }
  )
)

#' BinaryChromosome R6 Class
#' @export
BinaryChromosome <- R6::R6Class("BinaryChromosome",
  inherit = Chromosome,
  public = list(
    #' @description Initialize a new BinaryChromosome
    initialize = function(genes) {
      super$initialize(genes, type = "binary")
    }
  )
)

#' RealChromosome R6 Class
#' @export
RealChromosome <- R6::R6Class("RealChromosome",
  inherit = Chromosome,
  public = list(
    #' @description Initialize a new RealChromosome
    initialize = function(genes) {
      super$initialize(genes, type = "real")
    }
  )
)
