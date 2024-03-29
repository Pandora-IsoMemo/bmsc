% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_averaging.R
\name{get_avg_model}
\alias{get_avg_model}
\title{Compute model average}
\usage{
get_avg_model(models_input, weights)
}
\arguments{
\item{models_input}{Model list of class \code{\link{ConstrainedLinReg}}}

\item{weights}{Numeric vector of model weights}
}
\value{
Model that is model average due to model weigths
}
\description{
Computes model weights according to measure
}
