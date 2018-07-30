rm(list = ls())

require(smoof)
require(MOEADr)
require(devtools)
require(doParallel)
devtools::load_all()

# ======================================================================
# Simple input parameters
initial.sampling <- "lhs"
algo.runner <- "myalgo"
m0 <- 50
N0 <- 5
mi <- 10
Ni <- 1
elite.confs <- 10
summary.function <- "median"
model.order <- 3
model.type <- "ridge"
optimization.method = "Nelder-Mead"
budget <- 1000
ndigits <- 3
seed <- 1234
ncores <- 7


# ======================================================================
# List of tunable parameters

# The algorithm being tuned is a MOEA/D with the following configuration:
# - Decomposition method: SLD with h = 99
# - Scalar aggregation function: PBI (with tunable parameter theta.pbi)
# - Objective scaling: none
# - Neighborhood assignment: "by x" (with tunable parameter delta.p)
# - Variation stack:
#   - SBX recombination (with tunable parameters eta.x, p.x)
#   - Polynomial mutation (with tunable parameter eta.m and fixed p.m = 1/d)
#   - Simple truncation
# - Update strategy: restricted (with tunable parameter n.r)
# - Constraint handling: none
# - Termination criteria: Function calls with max.iter = 1000 * dim

parameters <- data.frame(name = c("theta.pbi",
                                  "delta.p",
                                  "eta.x",
                                  "p.x",
                                  "eta.m",
                                  "n.r"),
                         minx = c(0,  0.5, 0.5, 0.25, 0.5, 1),
                         maxx = c(10, 1.0, 100, 1.0,  100, 20))

# ======================================================================
# Build list of tuning instances
# Build function names (instances: UF1 - UF7, dimensions 3 - 40)
# Dimensions that are multiples of 3 are reserved for testing, all others are
# available for tuning.
fname   <- paste0("UF_", 1:7)
dims    <- sort(c(seq(from = 4, to = 40, by = 3),
                  seq(from = 5, to = 40, by = 3)))

allfuns <- expand.grid(fname, dims, stringsAsFactors = FALSE)

# Assemble instances list
tuning.instances <- vector(nrow(allfuns), mode = "list")

for (i in 1:length(tuning.instances)){
  fun.id <- as.numeric(strsplit(allfuns[i, 1], split = "_")[[1]][2])

  tuning.instances[[i]]$FUN   <- paste0(allfuns[i, 1], "_", allfuns[i, 2])
  tuning.instances[[i]]$alias <- paste0(allfuns[i, 1], " (", allfuns[i, 2], ")")
  fun <- do.call(paste0("makeUFFunction"),
                 args = list(dimensions = allfuns[i, 2],
                             id         = fun.id))
  myfun <- make_vectorized_smoof(prob.name  = "UF",
                                 dimensions = allfuns[i, 2],
                                 id         = fun.id)
  tuning.instances[[i]]$xmin  <- attr(fun,
                                      "par.set")$pars[[1]]$lower
  tuning.instances[[i]]$xmax  <- attr(fun,
                                      "par.set")$pars[[1]]$upper
  assign(x = tuning.instances[[i]]$FUN, value = myfun)
}

# ======================================================================
# Build target runner
myalgo <- function(instance, params){

  # Build MOEADr problem list
  problem <- list(name = instance$FUN,
                  xmin = instance$xmin,
                  xmax = instance$xmax,
                  m    = 2)

  # Load presets for the algorithm and include the tunable parameter values

  algo.preset <- preset_moead("original")

  algo.preset$aggfun <- list(name  = "pbi",
                             theta = params$config[1])

  algo.preset$neighbors$name <- "x"
  algo.preset$neighbors$delta.p <- params$config[2]

  algo.preset$variation[[1]]$etax <- params$config[3]
  algo.preset$variation[[1]]$px   <- params$config[4]
  algo.preset$variation[[2]]$etam <- params$config[5]
  algo.preset$variation[[2]]$pm   <- 1 / length(instance$xmin)

  algo.preset$update <- list(name = "restricted",
                             nr   = round(params$config[6]))

  algo.preset$stopcrit[[1]]$name <- "maxeval" # <-- type of stop criterion
  algo.preset$stopcrit[[1]]$maxeval <- 1000 * length(instance$xmin) # <-- set stop crit.


  out <- moead(problem  = problem,
               preset   = algo.preset,
               showpars = list(show.iters = "none"))

  # Read reference data to calculate the IGD
  Yref  <- as.matrix(read.table(paste0("data/pf_data/UF",
                                       strsplit(instance$FUN, "_")[[1]][2],
                                       ".dat")))
  IGD = calcIGD(Y = out$Y, Yref = Yref)

  # Return IGD as field "value" in the output list
  return(list(value = IGD))
}



out <- metatuner(parameters          = parameters,
                 tuning.instances    = tuning.instances,
                 algo.runner         = algo.runner,
                 elite.confs         = elite.confs,
                 budget              = budget,
                 m0                  = m0,
                 mi                  = mi,
                 initial.sampling    = initial.sampling,
                 ndigits             = ndigits,
                 N0                  = N0,
                 Ni                  = Ni,
                 summary.function    = summary.function,
                 model.type          = model.type,
                 model.order         = model.order,
                 optimization.method = optimization.method,
                 ncores              = ncores,
                 seed                = seed)

saveRDS(object = out, file = "./data/results001.rds")
