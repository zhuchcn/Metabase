% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-MetabolomicsSet-IO.R
\name{export_txt}
\alias{export_txt}
\title{Export mSet object to separate text files}
\usage{
export_txt(object, path, prefix, sep = "\\t", quote = FALSE,
  sample_names_head = "Sample name",
  feature_names_head = "Metabolite name")
}
\arguments{
\item{object}{An object that inherits from the \code{\link{mSet-class}}}

\item{path}{A string. Must be an exist directory to save the text files.}

\item{prefix}{A string to be prepend to the output file names. If is missing,
nothing will be prepended.}

\item{sep}{Can be either tab, comma, or any separater.}

\item{quote}{A logical value (TRUE or FALSE) whether add quotes to each column.
See \code{\link{write.table}}}

\item{sample_names_head}{A string. The name of the column for sample names.}

\item{feature_names_head}{A string. The name of the column for feature names.}
}
\description{
This function exports a \code{\link{mSet-class}} object into separate
text files (i.e. csv, tsv and txt).
}
\examples{
data(lipid)
dir.create("~/lipid-data")
export_txt(lipid, "~/lipid-data")
export_txt(lipid, "~/lipid-data", prefix = "lipid", sep = ",")

}
\author{
Chenghao Zhu
}
