% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_model.R
\name{cloneModel}
\alias{cloneModel}
\title{Clone an MRAmodel or MRAmodelSet object}
\usage{
cloneModel(old_model)
}
\arguments{
\item{old_model}{A MRAmodel or MRAmodelSet object.}
}
\value{
An MRAmodel/MRAmodelSet object with the same properties but separated from the old model
}
\description{
Copy a MRAmodel or MRAmodelSet object into a new independent variable
}
\author{
Bertram Klinger \email{bertram.klinger@charite.de}
#' @examples \dontrun{
clonedModel = cloneModel(mramodel)
}
}
