#' Evaluate candidate configurations on sets of instances
#'
#' Evaluates the candidate configurations listed on `configs.to.eval` on the
#' tuning instances listed in `instances.to.eval`. Notice that this routine does
#' not repeat evaluations - if a configuration `i` already has a performance
#' value for instance `j` that evaluation is skipped.
#'
#' @param tuning.instances list containing all available tuning instances.
#' @param instances.to.eval vector containing the indices of the instances to
#'                          be used.
#' @param config.list list containing all configurations sampled.
#' @param configs.to.eval vector containing the indices of the configurations to
#'                          be evaluated on the instances listed in
#'                          `instances.to.eval`.
#' @param algo.runner name of function used for evaluating the configurations.
#'                      See Section `Algorithm Runner` of [metatuner()]
#'                      documentation.
#'
#' @return Updated `config.list` containing performance evaluation of
#' `configs.to.eval` on `instances.to.eval`, as well as `nruns`, the number of
#' algorithm runs performed.
#'
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @export

EvaluateConfigurations <- function(tuning.instances,
                                   instances.to.eval = "all",
                                   config.list,
                                   configs.to.eval = "all",
                                   algo.runner){

  # Error checking
  assertthat::assert_that(is.list(tuning.instances),
                          length(tuning.instances) > 0,
                          is.numeric(instances.to.eval) ||
                            instances.to.eval == "all",
                          is.numeric(configs.to.eval) ||
                            configs.to.eval == "all",
                          is.character(algo.runner))

  if(identical(configs.to.eval, "all")) {
    configs.to.eval <- seq_along(config.list)
  }
  if(identical(instances.to.eval, "all")) {
    instances.to.eval <- seq_along(tuning.instances)
  }

  assertthat::assert_that(all(instances.to.eval > 0),
                          all(instances.to.eval <= length(tuning.instances)),
                          all(instances.to.eval == round(instances.to.eval)),
                          all(configs.to.eval > 0),
                          all(configs.to.eval <= length(config.list)),
                          all(configs.to.eval == round(configs.to.eval)))


  # ==========
  nruns <- 0
  Yij.all <- matrix(as.numeric(NA),
                    ncol = length(config.list),
                    nrow = length(tuning.instances))
  colnames(Yij.all) <- paste0("theta", seq_along(config.list))
  rownames(Yij.all) <- paste0("gamma", seq_along(tuning.instances))

  for (i in seq_along(configs.to.eval)){
    instances.seen <- config.list[[configs.to.eval[i]]]$Yij$instance.ID
    for (j in seq_along(instances.to.eval)){
      if(!(instances.to.eval[j] %in% instances.seen)){
        yij <- do.call(algo.runner,
                args = list(instance = tuning.instances[[instances.to.eval[j]]],
                            params   = config.list[[configs.to.eval[i]]]))
        config.list[[configs.to.eval[i]]]$Yij <-
          rbind(config.list[[configs.to.eval[i]]]$Yij,
                data.frame(instance.ID = instances.to.eval[j],
                           y           = yij))
        Yij.all[instances.to.eval[j], configs.to.eval[i]] <- yij
        nruns <- nruns + 1
      } else {
        indx <- which(config.list[[configs.to.eval[i]]]$Yij$instance.ID ==
                        instances.to.eval[j])
        Yij.all[instances.to.eval[j], configs.to.eval[i]] <-
          config.list[[configs.to.eval[i]]]$Yij[indx, 2]
      }
    }
  }

  # Return output
  outlist <- list(config.list = config.list,
                  Yij.all     = Yij.all,
                  nruns       = nruns)
  return(outlist)
}
