% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createFormula.R
\name{makePoly}
\alias{makePoly}
\title{Create polynomial of degree \code{maxExponent} from variable names}
\usage{
makePoly(vars, maxExponent, inverseExponent)
}
\arguments{
\item{vars}{character: variable names}

\item{maxExponent}{integer: highest exponent}

\item{inverseExponent}{integer: highest inverse exponent}
}
\value{
Character vector of \code{length(vars)} times \code{maxExponent}
}
\description{
Remark: Since this function is to be used only within
\code{\link{createFormula}}, the validity of the input is not checked here
but in \code{\link{createFormula}}.
}
\examples{
BMSC:::makePoly(vars = c("x1", "x2"), maxExponent = 3, inverseExponent = 2)
}
