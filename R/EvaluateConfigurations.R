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
#' @param config.list list vector containing field `A` (list of all
#'                    configurations sampled so far + additional info) as well
#'                    as other information regarding the performance of all
#'                    configurations tested and the process of MetaTuner.
#' @param configs.to.eval vector containing the indices of the configurations to
#'                          be evaluated on the instances listed in
#'                          `instances.to.eval`.
#' @param algo.runner name of function used for evaluating the configurations.
#'                      See Section `Algorithm Runner` of [metatuner()]
#'                      documentation.
#' @param summary.function name of function for aggregating the (scaled)
#' output values of `algo.runner` into a single performance value. Usual
#' functions include `mean` and `median`, but in principle any summarizing
#' function can be used.
#' @param parameters data frame containing the parameter names and bound
#' constraints.
#'
#' @return Updated `config.list` containing performance evaluation of
#' `configs.to.eval` on `instances.to.eval`, as well as `nruns`, the number of
#' algorithm runs performed.
#'
#' @importFrom foreach %dopar% %:%
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

EvaluateConfigurations <- function(tuning.instances,
                                   instances.to.eval = "all",
                                   config.list,
                                   configs.to.eval = "all",
                                   algo.runner,
                                   summary.function = "median",
                                   parameters){

  ## ==============
  ## Error checking done in the calling routine
  ## ==============

  if(identical(configs.to.eval, "all")) {
    configs.to.eval <- seq_along(config.list$A)
  }
  if(identical(instances.to.eval, "all")) {
    instances.to.eval <- seq_along(tuning.instances)
  }

  # ========== Prepare config.list fields
  npars   <- length(config.list$A[[1]]$config)
  nruns   <- config.list$nruns

  # Matrix of all performances
  if("Yij.all" %in% names(config.list)){
    Yij.all <- config.list$Yij.all
    diffcol <- length(config.list$A) - ncol(Yij.all)
    if (diffcol){
      Yij.all <- cbind(Yij.all,
                       matrix(as.numeric(NA),
                              ncol = diffcol,
                              nrow = length(tuning.instances)))
    }
  } else {
    Yij.all <- matrix(as.numeric(NA),
                      ncol = length(config.list$A),
                      nrow = length(tuning.instances))
  }
  colnames(Yij.all) <- paste0("config", seq_along(config.list$A))
  rownames(Yij.all) <- paste0("instance", seq_along(tuning.instances))

  # Data frame of configuration performances (with normalized parameters)
  config.perf <-
    as.data.frame(t(sapply(config.list$A,
                           function(x, params){x$config},
                           params = parameters)))
  names(config.perf) <- parameters$name


  # ========== Evaluate config/instance pairs that need to be evaluated

  # Define instance indices to be used by configurations to be evaluated
  instances.seen <- config.list$A[[configs.to.eval[1]]]$Yij$instance.ID
  inst <- which(!(instances.to.eval) %in% instances.seen)

  # Define set of instances to be visited
  inst.to.eval.parallel <- tuning.instances[instances.to.eval[inst]]

  if (length(inst.to.eval.parallel) > 0){

    # Define set of configs to evaluate
    confs.to.eval.parallel <- config.list$A[configs.to.eval]

    # Denormalize configurations to be used
    for(i in 1:length(confs.to.eval.parallel)){
      confs.to.eval.parallel[[i]]$config <- parameters$minx +
        confs.to.eval.parallel[[i]]$config * (parameters$maxx - parameters$minx)
    }

    # ========== Parallel evaluation
    matperfs <- foreach::foreach(i = inst.to.eval.parallel, .combine='rbind') %:%
      foreach::foreach(c = confs.to.eval.parallel, .combine='c') %dopar% {
        do.call(algo.runner, list(instance = i, params = c))
      }
    matperfs <- as.matrix(matperfs)
    if(ncol(matperfs) == 1) matperfs <- t(matperfs)

    # Store result from matperfs in config.list and Yij.all matrix
    for(i in 1:ncol(matperfs)) {
      for(j in 1:nrow(matperfs)){
        newrow <- data.frame(instance.ID = instances.to.eval[inst[j]],
                             y           = matperfs[j, i])
        names(newrow) <- names(config.list$A[[configs.to.eval[i]]]$Yij)

        saveRDS(object = list(config.list, configs.to.eval, i),
                file = "../tmprun.rds")

        config.list$A[[configs.to.eval[i]]]$Yij <-
          rbind(config.list$A[[configs.to.eval[i]]]$Yij, newrow)
        Yij.all[instances.to.eval[inst[j]],
                configs.to.eval[i]] <- matperfs[j, i][[1]]
      }
      rownames(config.list$A[[configs.to.eval[i]]]$Yij) <-
        seq(1:nrow(config.list$A[[configs.to.eval[i]]]$Yij))
    }

    nruns <- nruns + (ncol(matperfs) * nrow(matperfs))

    # Calculate normalized performances
    Yij.vals <- Yij.all[which(rowSums(!is.na(Yij.all)) != 0), ]
    instmins <- matrix(apply(Yij.vals, MARGIN = 1, FUN = min, na.rm = TRUE),
                       nrow = nrow(Yij.vals),
                       ncol = ncol(Yij.vals),
                       byrow = FALSE)
    instmaxs <- matrix(apply(Yij.vals, MARGIN = 1, FUN = max, na.rm = TRUE),
                       nrow = nrow(Yij.vals),
                       ncol = ncol(Yij.vals),
                       byrow = FALSE)
    Yij.norm <- (Yij.vals - instmins) / (instmaxs - instmins)

    # Update data frame of configuration performances
    config.perf <- cbind(config.perf,
                         perf = apply(X = Yij.norm,
                                      MARGIN = 2,
                                      FUN = summary.function,
                                      na.rm = TRUE))

    # Give each config its performance value
    for (i in seq_along(config.list$A)){
      config.list$A[[i]]$perf <- config.perf$perf[i]
    }

    # Return output
    config.list$Yij.all     <- Yij.all
    config.list$nruns       <- nruns
    config.list$Yij.norm    <- Yij.norm
    config.list$config.perf <- config.perf
  }
  return(config.list)
}
