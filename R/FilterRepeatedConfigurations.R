#' Filter redundant configurations
#'
#' Remove repeated configurations from the config.list
#'
#' @param newconfs new configurations returned by [OptimizeModels()]
#' @param config.list archive list
#'
#' @return updated list `newconfs` without redundant configurations
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'

FilterRepeatedConfigurations <- function(newconfs,
                                         config.list){

  isNew <- rep(TRUE, length(newconfs))
  for (i in seq(newconfs)){
    # Check for originality from config.list
    isNew[i] <- isNew[i] & !any(sapply(config.list$A,
                                       function(x, y){
                                         all(x$config == y)
                                       },
                                       y = newconfs[[i]]$config))

    # Check for repeated configurations in newconfs
    if(i > 1){
      isNew[i] <- isNew[i] & !any(sapply(newconfs[1:(i-1)],
                                         function(x, y){
                                           all(x$config == y)
                                         },
                                         y = newconfs[[i]]$config))
    }
  }

  # Return new configurations
  return(newconfs[isNew])
}
