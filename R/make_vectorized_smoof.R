#' Make vectorized smoof function
#'
#' Make a vectorized version of test functions available in package "smoof".
#'
#' This routine builds vectorized versions of the classic test functions
#' available in package `smoof`. Check `smoof`'s documentation for details.
#'
#' @param prob.name name of the problem to build
#' @param ... other parameters passed to each specific function
#'
#' @examples
#' \dontrun{
#'   library(smoof)
#'   Rosenbrock <- make_vectorized_smoof(prob.name = "Rosenbrock",
#'                                  dimensions   = 10)
#'   Rosenbrock(X = matrix(runif(100), ncol = 10))
#' }
#'
#' @export

make_vectorized_smoof <- function(prob.name, ...){

  if(!("smoof" %in% rownames(utils::installed.packages()))){
    stop("Please install package 'smoof' to continue")
  } else {

    my.args            <- as.list(sys.call())[-1]
    my.args$prob.name  <- NULL
    if (length(my.args) == 0) my.args <- list()

    myfun <- do.call(utils::getFromNamespace(x = paste0("make",
                                                        prob.name,
                                                        "Function"),
                                             ns = "smoof"),
                     args = my.args)
    myfun2 <- function(X, ...){
      m <- attr(myfun, "n.objectives")
      if (m == 1){
        if(is.matrix(X)){
          Y <- as.vector(apply(X,
                               MARGIN = 1,
                               FUN = myfun))
        } else{
          Y <- myfun(X)
        }
      } else {
        if(is.matrix(X)){
          y <- t(apply(X,
                       MARGIN = 1,
                       FUN = myfun))
        } else{
          y <- myfun(X)
        }
        Y <- matrix(y, ncol = m, byrow = FALSE)
      }
      return(Y)
    }
  }
}

