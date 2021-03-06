% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profile_likelihood.R
\name{profileLikelihood}
\alias{profileLikelihood}
\title{Computes the profile likelihood and the parameters relationships of each parameters in the model}
\usage{
profileLikelihood(model_description, nb_points = 10000, nb_cores = 1)
}
\arguments{
\item{model_description}{An MRAmodel object}

\item{nb_points}{Number of points to plot the profile}

\item{nb_cores}{Maximum number of cores used for the calculation}
}
\value{
Returns a list of the profile for each parameters
}
\description{
Computes the profile likelihood and the parameters relationships of each parameters in the model
}
\author{
Mathurin Dorel \email{dorel@horus.ens.fr}
}
