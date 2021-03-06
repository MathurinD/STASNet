% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_model.R
\name{createModelSet}
\alias{createModelSet}
\title{Build an MRAmodelSet}
\usage{
createModelSet(model_links, basal_file, csv_files, var_files = c(),
  nb_cores = 1, inits = 1000, perform_plots = F, method = "geneticlhs",
  unused_perturbations = c(), unused_readouts = c(), MIN_CV = 0.1,
  DEFAULT_CV = 0.3, model_name = "default", rearrange = "bystim")
}
\arguments{
\item{model_links}{Path to the file containing the network structure, either in matrix form or in list of links form. Extension .tab expected}

\item{basal_file}{Path to the file indicating the nodes without basal activity. Extension .dat expected.}

\item{nb_cores}{Number of cores that should be used for the computation}

\item{inits}{Number of initialisation steps which should be performed (see method for the exact meaning of this value)}

\item{perform_plots}{Whether the distribution of the residuals and the correlation plots for the parameters deduced by correlation should be plotted or not}

\item{method}{Method to be used for the initialisation, available methods are :
random : Perform a Latin Hypercube Sampling to choose \emph{inits} starting points then perform a gradient descent to find the local optimum for each of those points.
correlation : Deduce some parameters from the correlation between the measurements for the target node and all of its input nodes, then perform random to find the other parameters. Recommended, very efficient for small datasets.
genetic : Genetic algorithm with mutation only. \emph{inits} is the total number of points sampled.
annealing : Simulated annealing and gradient descent on the best result. \emph{inits} is the maximum number of iteration without any change before the algorithm decides it reached the best value. Use not recommended.}

\item{unused_perturbations}{Perturbations in the dataset that should not be used}

\item{unused_readouts}{Measured nodes in the datasets that should not be used}

\item{MIN_CV}{Minimum coefficient of variation.}

\item{DEFAULT_CV}{Default coefficient of variation to use when none is provided and there are no replicated in the data.}

\item{model_name}{The name of the model is derived from the name of the data.stimulation file name. If data.stimulation is a matrix or a data.frame, 'model_name' will be used to name the model.}

\item{rearrange}{Whether the rows should be rearranged. "no" to keep the order of the perturbations from the data file, "bystim" to group by stimulations, "byinhib" to group by inhibitions.}
}
\description{
Build and fit an MRAmodelSet, which consists of the simultaneous fitting of several MRA models
}
\author{
Mathurin Dorel \email{dorel@horus.ens.fr}
}
