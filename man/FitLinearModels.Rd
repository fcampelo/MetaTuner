% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FitLinearModels.R
\name{FitLinearModels}
\alias{FitLinearModels}
\title{Fit linear regression models}
\usage{
FitLinearModels(X, Nmodels, model.order, weights)
}
\arguments{
\item{X}{data frame containing the performance data}

\item{Nmodels}{number of models to generate (1 original plus \code{Nmodels - 1}
perturbations)}

\item{model.order}{order to use for the regression model. Defaults to 2}

\item{weights}{vector of observation weights returned by \code{\link[=GetWeights]{GetWeights()}}.}
}
\value{
list containing information about all models fitted
}
\description{
Fits linear regression models to the performance data gathered by MetaTuner.
}
\author{
Felipe Campelo (\email{fcampelo@ufmg.br}),
Athila Trindade (\email{rochaathila@gmail.com})
}
