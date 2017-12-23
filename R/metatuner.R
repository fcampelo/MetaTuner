
metatuner <- function(algo.runner,
                      parameters,
                      tuning.instances,
                      initial.sampling,
                      m0,
                      N0){

  # Error checking and initial definitions

  # ===========

  A <- GenerateInitialSample(m0     = m0,
                             dim    = nrow(parameters),
                             method = initial.sampling)

  Gamma.A <- SampleInstances(instance.list = tuning.instances,
                             N = N0)

}
