% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModelFit.R
\name{prepModelNames}
\alias{prepModelNames}
\title{Extract model names from model objects}
\usage{
prepModelNames(models)
}
\arguments{
\item{models}{List with models of class \code{\link{ConstrainedLinReg}}}
}
\description{
Extracts the model formulae from a list of model objects of
class \code{\link{ConstrainedLinReg}}. Elements that are superfluous for
reading (e.g., brackets) are removed.
}
