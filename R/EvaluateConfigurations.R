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
#' constraints
#'
#' @return Updated `config.list` containing performance evaluation of
#' `configs.to.eval` on `instances.to.eval`, as well as `nruns`, the number of
#' algorithm runs performed.
#'
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

  # ========== Error checking
  assertthat::assert_that(is.list(tuning.instances),
                          length(tuning.instances) > 0,
                          is.numeric(instances.to.eval) ||
                            instances.to.eval == "all",
                          is.numeric(configs.to.eval) ||
                            configs.to.eval == "all",
                          is.character(algo.runner), length(algo.runner) == 1,
                          is.character(summary.function),
                          length(summary.function) == 1)

  if(identical(configs.to.eval, "all")) {
    configs.to.eval <- seq_along(config.list$A)
  }
  if(identical(instances.to.eval, "all")) {
    instances.to.eval <- seq_along(tuning.instances)
  }

  assertthat::assert_that(all(instances.to.eval > 0),
                          all(instances.to.eval <= length(tuning.instances)),
                          all(instances.to.eval == round(instances.to.eval)),
                          all(configs.to.eval > 0),
                          all(configs.to.eval <= length(config.list$A)),
                          all(configs.to.eval == round(configs.to.eval)))


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
  # PARALELL-IZE HERE
  # VVVVVVVVVVVVVVVV
  for (i in seq(config.list$A)){
    if (i %in% configs.to.eval){
      # ^^ should the config be evaluated?

      instances.seen <- config.list$A[[i]]$Yij$instance.ID
      for (j in seq_along(instances.to.eval)){
        if(!(instances.to.eval[j] %in% instances.seen)){
          # ^^ hasn't the instance been evaluated already?

          # denormalize configuration
          myconf <- config.list$A[[i]]
          myconf$config <- parameters$minx +
            myconf$config * (parameters$maxx - parameters$minx)

          # call algorithm runner
          yij <- do.call(algo.runner,
                         args = list(instance = tuning.instances[[instances.to.eval[j]]],
                                     params   = myconf))

          # Store result in config.list
          config.list$A[[i]]$Yij <-
            rbind(config.list$A[[i]]$Yij,
                  data.frame(instance.ID = instances.to.eval[j],
                             y           = yij))
          Yij.all[instances.to.eval[j], i] <- yij

          # update run counter
          nruns <- nruns + 1
        }
      }
    }
  }

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
  return(config.list)
}
