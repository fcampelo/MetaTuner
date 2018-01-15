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
#' @param ndigits number of decimal places to use for each parameter.
#' @param ... further parameters to be passed down to the specific methods (see
#'            section `Methods` for details).
#'
#' @return a list vector where each object is a configuration list,
#' containing the following fields:
#'     - `config`, a named list containing parameter values
#'     - `Yij`, a data frame with two columns: `instance.ID` and `y`
#'        (the performance value of the configuration on the instance).
#'        This data frame is generated empty.
#'     - `perf`, a numeric scalar containing the summary performance value
#'       of the configuration
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @examples
#' myconfs <- GenerateInitialSample(m0 = 20, dim = 2,
#'                                  method = "sobol") # Using LDSP
#' myconfs <- GenerateInitialSample(m0 = 100, dim = 5,
#'                                  method = "lhs") # Using LHS
#' configs <- as.data.frame(t(sapply(myconfs,
#'                                   function(x){x$config})))
#' pairs(configs, pch = 20)

GenerateInitialSample <- function(m0,
                                  dim,
                                  method = c("lhs", "sobol"),
                                  ndigits = 4,
                                  ...){

  if(length(ndigits) == 1) ndigits <- rep(ndigits, times = dim)

  ## ==============
  ## Error checking done in the calling routine
  ## ==============

  if (method == "lhs"){
    mysample <- lhs::randomLHS(n = m0, k = dim)
  }

  if (method == "sobol"){
    mysample <- SobolSequence::sobolSequence.points(dimR = dim, count = m0)
  }

  for (j in 1:ncol(mysample)){
    mysample[, j] <- round(mysample[, j], digits = ndigits[j])
  }

  outlist <- apply(X      = mysample,
                   MARGIN = 1,
                   FUN    = function(x){
                     list(config = x,
                          Yij    = data.frame(instance.ID = character(),
                                              y           = numeric(),
                                              stringsAsFactors = FALSE),
                          perf   = NA)})

  return(outlist)
}
