% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-LipidomicsSet.R
\name{nDoubleBonds}
\alias{nDoubleBonds}
\title{Get the number of double bonds}
\usage{
nDoubleBonds(x)
}
\arguments{
\item{x}{character. The lipid annotation name. Must be formatted by the
\code{\link{lipid_name_formater}} function.}
}
\description{
This function returns the number of double bonds from a given
lipid annotation name. For example, the number of double bonds of lipid PC 34:2
is 34. The lipid annotation names input must be the ouput of
\code{\link{lipid_name_formater}} function.
}
\author{
Chenghao Zhu
}
