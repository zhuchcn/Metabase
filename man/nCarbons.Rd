% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-LipidomicsSet.R
\name{nCarbons}
\alias{nCarbons}
\title{Extract the carbon chain length from lipidome annotation}
\usage{
nCarbons(x)
}
\arguments{
\item{x}{character. The lipid annotation name. Must be formatted by the
\code{\link{lipid_name_formater}} function.}
}
\description{
This function returns the total number of carbons from a given
lipid annotation name. For example, the carbon chain length of lipid PC 34:2
is 34. The lipid annotation names input must be the ouput of
\code{\link{lipid_name_formater}} function.
}
\author{
Chenghao Zhu
}
