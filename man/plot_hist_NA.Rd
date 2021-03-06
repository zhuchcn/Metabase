% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-plot.R
\name{plot_hist_NA}
\alias{plot_hist_NA}
\title{Plot Histogram of Missing Values}
\usage{
plot_hist_NA(object, include.zero = FALSE)
}
\arguments{
\item{object}{A \code{\link{mSet-class}} or derived object.}

\item{include.zero}{Logic value whether to include zero in the histogram.
Default is FALSE.}
}
\value{
A ggplot object
}
\description{
This function takes a mSet object and print a histogram of
missing values. The x axis is the number of missing values per feature, and
the y axis is number of features with specific number of missing values.
This is usful when filling NAs.
}
\examples{
# ADD_EXAMPLES_HERE
}
