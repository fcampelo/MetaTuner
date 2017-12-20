#' Generates the initial sampling of the space of configurations
#'
#' Generates an initial sample within the space of configurations using a
#' space-covering design. The initial sample is scaled to the interval `[0,1]`
#' for all parameters
#'
#' @section Methods:
#' Currently there are two methods implemented for the generation of the initial
#' sample:
#' - Latin hypercube sampling ("lhs"): a space-covering design based on Latin
#'   Hypercube Sampling. Needs no additional parameters. Needs no additional
#'   parameters.
#' - Low-discrepancy sequences of points ("sobol"): a space-covering design
#'   using Sobol's low-discrepancy sequences of points. Based on the
#'   implementation of package `SobolSequence`. Needs no additional parameters.
#'
#' @param m0 number of points to be generated
#' @param dim dimension for generating the sample (i.e., number of parameters
#'            being tuned)
#' @param method type of method to be used in the generation of the sample (see
#'               section `Methods` for details).
#' @param ... further parameters to be passed down to the specific methods (see
#'            section `Methods` for details).
#'
#' @return a matrix where each line represents one configuration sampled from
#' the space of possible configurations.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @export
#'
#' @examples
#' myconfs <- GenerateInitialSample(m0 = 20, dim = 2,
#'                                  method = "lhs") # Using LHS
#' plot(myconfs[, 1], myconfs[, 2], type = "p", pch = 20)
#' myconfs <- GenerateInitialSample(m0 = 50, dim = 5,
#'                                  method = "sobol") # Using Sobol
#' pairs(as.data.frame(myconfs))

GenerateInitialSample <- function(m0,
                                  dim,
                                  method = c("lhs", "sobol"),
                                  ...){

  # Error checking
  method <- match.arg(method, c("lhs", "sobol"))
  assertthat::assert_that(assertthat::is.count(m0),
                          assertthat::is.count(dim))

  if (method == "lhs"){
    mysample <- lhs::randomLHS(n = m0, k = dim)
  }

  if (method == "sobol"){
    mysample <- SobolSequence::sobolSequence.points(dimR = dim, count = m0)
  }

  return(mysample)
}
