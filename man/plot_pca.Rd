% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-plot.R
\name{plot_pca}
\alias{plot_pca}
\title{Create a PCA plot}
\usage{
plot_pca(object, color, ellipse = FALSE, ...)
}
\arguments{
\item{object}{an mSet object}

\item{color}{character variable indicating which sample variable will be used
to give colors. If length is larger than 1, the two variables will be used
by calling \code{\link{interaction}}}

\item{ellipse}{boolean whether to draw ellipses.}
}
\description{
Create a PCA plot of an mSet object
}
\examples{

plot_pca(lipid)
plot_pca(lipid, color = "Treatment")
plot_pca(lipid, color = c("Treatment", "Timepoint"), ellipse = TRUE)

}
\seealso{
\code{\link[ggmetaplots]{ggscatterplot}}
}
