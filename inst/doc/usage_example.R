## ------------------------------------------------------------------------
suppressPackageStartupMessages(require(smoof))
suppressPackageStartupMessages(require(ExpDE))
suppressPackageStartupMessages(require(MetaTuner))

## ------------------------------------------------------------------------
parameters <- data.frame(name = c("f", "cr"),
minx = c(0.1, 0),
maxx = c(5, 1))

## ------------------------------------------------------------------------
# We want all even dimensions of the Rosenbrock function between 2 and 100
fname   <- "Rosenbrock"
dims    <- seq(from = 2, to = 100, by = 2)
allfuns <- expand.grid(fname, dims, stringsAsFactors = FALSE)

# Assemble instances list
tuning.instances <- vector(nrow(allfuns), mode = "list")

# For each position in vector `tuning.instances`:
for (i in 1:length(tuning.instances)){

# Name of function to call for instance
tuning.instances[[i]]$FUN   <- paste0(allfuns[i,1], "_", allfuns[i,2])

# Instance alias
tuning.instances[[i]]$alias <- paste0(allfuns[i,1], " (", allfuns[i,2], ")")

# Build vectorized version of the original implementation from `smoof`
myfun <- make_vectorized_smoof(prob.name = fname,
dimension = dims[i])

# Also build the "un-vectorized" version, we'll need some of its attributes
fun   <- do.call(paste0("make", fname, "Function"),
args = list(dimensions = dims[i]))

# Extract lower and upper bounds for the optimization variables
tuning.instances[[i]]$xmin  <- attr(fun, "par.set")$pars[[1]]$lower
tuning.instances[[i]]$xmax  <- attr(fun, "par.set")$pars[[1]]$upper

# Assign the vectorized function to the name provided in `tuning.instances`.
assign(x = tuning.instances[[i]]$FUN, value = myfun)
}

## ------------------------------------------------------------------------
myalgo <- function(instance, params){
  D        <- length(instance$xmin) # Get problem dimension
  popsize  <- 5 * D                # Define population size
  
  # ExpDE's input for problem parameters
  probpars <- list(name = instance$FUN,
                   xmin = instance$xmin,
                   xmax = instance$xmax)
  
  # Mutation parameters
  mutpars  <- list(name = "mutation_rand", f = params$config[1])
  
  # Recombination parameters
  recpars  <- list(name = "recombination_bin",
                   cr   = params$config[2], nvecs = 1)
  
  # Other fixed parameters for ExpDE
  selpars  <- list(name = "selection_standard")
  showpars <- list(show.iters = "dots", showevery = 10)
  stopcrit <- list(names = "stop_maxiter", maxiter = 100)
  
  # Run DE on instance
  out <- ExpDE(popsize = popsize,
               mutpars = mutpars, recpars = recpars,
               selpars = selpars, stopcrit = stopcrit,
               probpars = probpars, showpars = showpars)
  
  # Return quality value (smaller = better)
  return(out$Fbest)
}

## ------------------------------------------------------------------------
m0               <- 10     # initial number of candidate configurations
mi               <- 5      # number of additional configurations per iteration
initial.sampling <- "lhs"  # initial sampling method
ndigits          <- 2      # number of digits to consider for each parameter
elite.confs      <- m0 / 2 # number of elite configurations to keep
N0               <- 5      # initial number of instances to sample
Ni               <- 1      # additional instances per iteration
budget           <- 200    # number of algorithm runs to use

summary.function    <- "median"   # summarization function for performance
model.type          <- "quantile" # type of regression to use
model.order         <- 3          # order of the polynomial used for regression
optimization.method <- "Nelder-Mead" # optimization method to use

## ---- echo = FALSE, eval = TRUE, cache = TRUE----------------------------
output <- MetaTuner::metatuner(parameters, tuning.instances, "myalgo",
                    m0, mi, initial.sampling, ndigits, elite.confs,
                    N0, Ni, summary.function,
                    model.type, model.order,
                    optimization.method,
                    budget)

## ---- echo = FALSE, eval = TRUE------------------------------------------
# Check the elite configurations found
output$elite.confs

