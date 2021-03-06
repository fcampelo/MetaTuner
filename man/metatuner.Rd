% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metatuner.R
\name{metatuner}
\alias{metatuner}
\title{Metaheuristics tuner}
\usage{
metatuner(parameters, tuning.instances, algo.runner, elite.confs, budget,
  m0 = 3 * nrow(parameters), mi = 5L, initial.sampling = "lhs",
  ndigits = 3L, N0 = 5L, Ni = 1L, summary.function = "median",
  model.type = "quantile", model.order = 3L,
  optimization.method = "Nelder-Mead", ncores = 1L,
  seed = as.integer(Sys.time()))
}
\arguments{
\item{parameters}{data frame containing the parameter names and bound
constraints. See Section \emph{Tunable Parameters} for details.}

\item{tuning.instances}{list of instances available for tuning. See Section
\emph{Tuning Instances} for details.}

\item{algo.runner}{name of function used for evaluating the configurations.
See Section \emph{Algorithm Runner} for details.}

\item{elite.confs}{number of elite configurations to maintain at each
iteration.}

\item{budget}{number of algorithm runs allocated for the tuning effort.}

\item{m0}{initial number of configurations to be generated. Defaults to
\code{3 * nrow(parameters)}.}

\item{mi}{number of new configurations to generate at each iteration.
Defaults to \code{5}.}

\item{initial.sampling}{type of method to be used in the generation of the
initial sample. See Section \emph{Initial Sampling Methods} for details.
Defaults to "lhs".}

\item{ndigits}{integer with the number of digits to use as the resolution of
each parameter (i.e., the parameter will have \code{(10 ^ ndigits)}
possible values during the tuning process).
Accepts a vector input, if different resolutions are desired for
different parameters. Defaults to \code{3}.}

\item{N0}{initial number of instances to sample. Defaults to \code{5}.}

\item{Ni}{number of new instances to add at each iteration. Defaults to \code{1}.}

\item{summary.function}{name of function for aggregating the (scaled)
output values of \code{algo.runner} into a single performance value. Usual
values include "mean" and "median", but in principle any
summarizing function can be used.}

\item{model.type}{Type of regression model to use. See Section
\emph{Statistical Models} for details.}

\item{model.order}{Order of the model to fit. Defaults to \code{3}.}

\item{optimization.method}{optimization method to use for estimating new
configurations. See Section \emph{Optimization Methods} for details.
Defaults to "Nelder-Mead".}

\item{ncores}{number of processor cores to use. Receives either an integer or
the value "max", in which case the method uses all available cores,
minus one.}

\item{seed}{seed for the random number generator. Use a scalar integer value.}
}
\description{
Search for parameter configurations that are
expected to yield the best performance for a given optimizer on instances
belonging to the same problem class as those used in the tuning effort.
Currently the method only works for parameters that are box-constained,
continuous, and Real.
}
\section{Tunable Parameters}{

The parameters to be tuned are passed to \code{metatuner} as a data frame
containing three columns:
\itemize{
\item \code{name}, with a unique name for each parameter.
\item \code{minx}, with the smallest allowed value for each parameter.
\item \code{maxx}, with the largest allowed value for each parameter.
}
}

\section{Tuning Instances}{

The problem instances available for the tuning effort must be passed as
list vector \code{tuning.instances}, in which each position contains the following
fields:
\itemize{
\item \code{FUN}, the name of the function that implements the instance. Must
have a formal argument \code{X} (e.g., \code{myfun <- function(X){...}}),
which can be either a numeric vector or a matrix of row vectors.
\item \code{xmin}, the lower bound for the optimization variables
\item \code{xmax}, the upper bound for the optimization variables
\item \code{alias} (optional), an alias for the instance
}
}

\section{Algorithm Runner}{

The \code{algo.runner} parameter points to a function that receive the instance
configuration (i.e., one element of the list vector \code{tuning.instance}) and
a numeric vector with the values of the tunable parameters (e.g.,
\code{myalgo <- function(instance, params){...}}), and return a single scalar
quantifying the performance of the algorithm equipped with that configuration
for that particular instance.

\strong{IMPORTANT}: \code{metatuner} assumes one wants to \emph{minimize} the expected
output of \code{algo.runner}. When working with quality indicators that should be
maximize, please do not forget to multiply this output by \code{-1} in the
output of \code{algo.runner}.
}

\section{Initial Sampling Methods}{

\code{metatuner} generates the initial sampling of the space of configurations
using the method given in \code{initial.sampling}. Currently available methods
are:
\itemize{
\item Latin hypercube sampling ("lhs"): a space-covering design based on Latin
Hypercube Sampling (uses package \code{lhs}).
\item Low-discrepancy sequences of points ("sobol"): a space-covering design
using Sobol's low-discrepancy sequences of points (uses package
\code{SobolSequence}).
}
}

\section{Statistical Modeling}{

Parameter \code{model.type} informs \code{metatuner} of the type of regression model to
fit. Currently implemented alternatives are:
\itemize{
\item \code{linear} for linear regression using OLS.
\item \code{quantile} for quantile regression (uses package \code{quantreg}).
\item \code{lasso} for lasso regression (uses package \code{hqreg})
\item \code{ridge} for ridge regression (uses package \code{hqreg})
}

In all cases \code{metatuner} fits a polynomial model to the performance data
gathered from running the candidate configurations on the tuning instances.
Parameter \code{model.order} is used to inform the (maximum) order of the
polynomial model to be fitted.
}

\section{Optimization Methods}{

\code{metatuner} uses \code{\link[stats:constrOptim]{stats::constrOptim()}} to optimize the predicted quality of
\code{algo.runner} with regards to its tunable parameters. Any method that does
not require an explicit function for returning gradient / Hessian information
can be used as \code{optimization.method}. Currently we recommend using
\code{Nelder-Mead}, but \code{SANN} is also a (usually slower) possibility.
}

