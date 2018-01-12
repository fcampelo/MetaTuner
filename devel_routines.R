rm(list = ls())

require(smoof)
require(ExpDE)
require(devtools)
devtools::load_all()

# ======================================================================
# Simple input parameters
initial.sampling.method <- "lhs"
algo.runner <- "myalgo"
m0 <- 10
N0 <- 5
mi <- 3
Ni <- 1
elite.confs <- 5
summary.function <- "median"
model.order <- 2
model.type <- "quantile"
optimization.method = "Nelder-Mead"
budget <- 200
parameter.resolution <- c(3, 4)

# ======================================================================
# List of tunable parameters
parameters <- data.frame(name = c("f", "cr"),
                         minx = c(0.1, 0),
                         maxx = c(5, 1))

# ======================================================================
# Build list of tuning instances (Rosenbrock function, dim = 2:30)
fname   <- "Rosenbrock"
dims    <- 2:30
allfuns <- expand.grid(fname, dims, stringsAsFactors = FALSE)

# Assemble instances list
tuning.instances <- vector(nrow(allfuns), mode = "list")
for (i in 1:length(tuning.instances)){
  tuning.instances[[i]]$FUN   <- paste0(allfuns[i,1], "_", allfuns[i,2])
  tuning.instances[[i]]$alias <- paste0(allfuns[i,1], " (", allfuns[i,2], ")")
  fun <- do.call(paste0("make", fname, "Function"),
                 args = list(dimensions = dims[i]))
  myfun <- make_vectorized_smoof(prob.name = fname,
                                 dimension = dims[i])
  tuning.instances[[i]]$xmin  <- attr(fun,
                                      "par.set")$pars[[1]]$lower
  tuning.instances[[i]]$xmax  <- attr(fun,
                                      "par.set")$pars[[1]]$upper
  assign(x = tuning.instances[[i]]$FUN, value = myfun)
}

# ======================================================================
# Build target runner
myalgo <- function(instance, params){
  D        <- length(instance$xmin)
  popsize  <- 10 * D
  probpars <- list(name = instance$FUN,
                   xmin = instance$xmin,
                   xmax = instance$xmax)
  mutpars  <- list(name = "mutation_rand", f = params$config[1])
  recpars  <- list(name = "recombination_bin",
                   cr   = params$config[2], nvecs = 1)
  selpars  <- list(name = "selection_standard")
  showpars <- list(show.iters = "dots", showevery = 10)
  stopcrit <- list(names = "stop_maxiter", maxiter = 100)
  out <- ExpDE(popsize = popsize,
               mutpars = mutpars, recpars = recpars,
               selpars = selpars, stopcrit = stopcrit,
               probpars = probpars, showpars = showpars)
  return(out$Fbest)
}



