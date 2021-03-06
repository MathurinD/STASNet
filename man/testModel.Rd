% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_manipulation.R
\name{testModel}
\alias{testModel}
\title{Computes the fitting scores for a new parameter set}
\usage{
testModel(mra_model, new_parameters, refit_model = FALSE)
}
\arguments{
\item{mra_model}{The MRAmodel object for which the score should be computed}

\item{new_parameters}{A vector of parameters to use for the new fit}

\item{refit_model}{Whether the model should be refitted before computing the scores (using the 'mra_model$parameters' as the initial value)}
}
\value{
An objet of class MRAmodel
}
\description{
Test the model with the provided parameter set and returns the fit, the scores and the (possibly updated) parameter set
}
