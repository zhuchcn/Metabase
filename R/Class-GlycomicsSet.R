################################################################################
#' @title Deprecated S4 class to store experiment data for glycomics experiment
#' This is a S4 class inherits from the virtual class
#' \code{\link{ExperimentData-class}}. The purpose of this class is to store
#' all the additional experiment information from a glycomics experiment.
#' This class has a list like structure. It contains five slots and each is
#' either a character or numeric value.
#' @slot institute The institute or lab where the experiment was conducted.
#' @exportClass GlycomicsExperimentData
setClass(
    Class = "GlycomicsExperimentData",
    representation = representation(
        institute = "characterOrNULL"
    ),
    contain = "ExperimentData",
    prototype = prototype(
        institute = NULL
    )
)

################################################################################
# setClassUnion("GlycomicsExperimentDataOrNULL",
#               c("GlycomicsExperimentData", "NULL"))

################################################################################
#' @title Deprecated Construction method for the S4 class GlycomicsExperimentData
#' @description
#' This function will be deprecated in the future
#' The construction method to build a
#' \code{\link{GlycomicsExperimentData-class}} object.
#' @param institute Character indicating the institute or lab where the experiment was conducted.
#' @export
GlycomicsExperimentData = function(
    institute = NULL
){
    new("GlycomicsExperimentData",
        institute = institute)
}

################################################################################
#' @name GlycomicsSet-class
#' @title S4 class to store glycomics experiment data
#'
#' @description
#' This is a S4 class inherits from the virtual
#' \code{\link{mSet-class}} to store the glycomics data. This class acts
#' as a data container for the glycomics data. Once data is cleaned and
#' passed into the class, it is ready to do statistical analysis and
#' visualization.
#'
#' @slot conc_table A \code{\link{conc_table-class}} object that stores the
#' concentration information from the experiment. The column names should be
#' the feature IDs, and the rownames should be the row IDs. This should be a
#' numeric matrix.
#'
#' @slot sample_table A \code{\link{sample_table-class}} object that stores the
#' sample meta-data information. The row names should be sample IDs and
#' should match the column names of the conc_table.
#'
#' @slot feature_data A \code{\link{feature_data-class}} object that stores the
#' feature infromation during the experiment. The row names should be feature
#' IDs and should match the row names of the conc_table.
#'
#' @slot experiment_data A list contains additional experiment information.
#'
#' @exportClass GlycomicsSet
#' @author Chenghao Zhu
setClass( Class = "GlycomicsSet", contains = "mSet" )
################################################################################
#' @name GlycomicsSet
#' @title Construct a GlycomicsSet object
#' @description This is the method to construct a
#' \code{\link{GlycomicsSet-class}} object.
#'
#' @param conc_table A \code{\link{conc_table-class}} object that stores the
#' concentration information from the experiment. The column names should be
#' the feature IDs, and the rownames should be the row IDs. This should be a
#' numeric matrix.
#'
#' @param sample_table A \code{\link{sample_table-class}} object that stores the
#' sample meta-data information. The row names should be sample IDs and
#' should match the column names of the conc_table.
#'
#' @param feature_data A \code{\link{feature_data-class}} object that stores the
#' feature infromation during the experiment. The row names should be feature
#' IDs and should match the row names of the conc_table.
#'
#' @param experiment_data A list contains additional experiment information.
#'
#' @export
#' @author Chenghao Zhu
#' @export
GlycomicsSet = function(
    conc_table      = NULL,
    sample_table    = NULL,
    feature_data    = NULL,
    experiment_data = NULL
){
    new("GlycomicsSet",
        conc_table      = conc_table,
        sample_table    = sample_table,
        feature_data    = feature_data,
        experiment_data = experiment_data)
}
################################################################################
#' @export
setMethod(
    "show", signature = "GlycomicsSet",
    definition = function(object){
        cat(str_pad(">>>>>> Glycomics Experiment <<<<<<", 54, "left", " "))
        cat("\n")
        callNextMethod()
    }
)
