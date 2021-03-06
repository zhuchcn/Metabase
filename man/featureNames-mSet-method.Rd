% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-mSet.R
\docType{methods}
\name{featureNames,mSet-method}
\alias{featureNames,mSet-method}
\alias{featureNames<-,mSet-method}
\alias{featureNames<-}
\title{Get the feature names of a mSet object}
\usage{
\S4method{featureNames}{mSet}(x)

\S4method{featureNames}{mSet}(x) <- value
}
\arguments{
\item{x}{A \code{\link{mSet-class}} or derived class object}

\item{value}{A character vector. The length must equal to the number of
features of the object.}
}
\value{
A character vector.
}
\description{
Get the feature names from a given \code{\link{mSet-class}}
or derived classes.
}
\seealso{
\code{\link{mSet-class}}, \code{\link{conc_table-class}},
\code{\link{feature_data-class}}, \code{\link{MetabolomicsSet-class}}
}
