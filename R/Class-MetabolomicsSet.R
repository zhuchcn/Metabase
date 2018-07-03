################################################################################
#' @name MetabolomicsExperimentData-class
#' @title S4 class to store experiment data for metabolomics experiment.
#' @description
#' This is a S4 class inherits from the virtual class
#' \code{\link{ExperimentData-class}}. The purpose of this class is to store
#' all the additional experiment information from a metabolomics experiment.
#' This class has a list like structure. It contains five slots and each is
#' either a character or numeric value.
#'
#' @slot institute character indicates the institute where the experiment was
#' conducted
#'
#' @slot instument character indicates the instument that the samples were ran
#'
#' @slot resuspension_volumn_ul numeric value, the volumn that samples were
#' resuspended, in ul.
#'
#' @slot injection_volumn_ul numeric value, the volumn injected into the
#' instrument, in ul.
#'
#' @slot data_precessing_software character indicates the software the data
#' processing was done.
#'
#' @exportClass MetabolomicsExperimentData-class
#' @author Chenghao Zhu
setClass(
    Class = "MetabolomicsExperimentData",
    representation = representation(
        institute                = "characterOrNULL",
        instrument               = "characterOrNULL",
        resuspension_volumn_ul   = "numericOrNULL",
        injection_volumn_ul      = "numericOrNULL",
        data_processing_software = "characterOrNULL"
    ),
    contain = "ExperimentData",
    prototype = prototype(
        institute                = NULL,
        instrument               = NULL,
        resuspension_volumn_ul   = NULL,
        injection_volumn_ul      = NULL,
        data_processing_software = NULL
    )
)
################################################################################
#' @name MetabolomicsExperimentData
#' @title Construction method for MetabolomicsExperimentData-class
#' @description The construction method to build a
#' \code{\link{MetabolomicsExperimentData-class}} object.
#'
#' @param institute character indicates the institute where the experiment was
#' conducted
#'
#' @param instument character indicates the instument that the samples were ran
#'
#' @param resuspension_volumn_ul numeric value, the volumn that samples were
#' resuspended, in ul.
#'
#' @param injection_volumn_ul numeric value, the volumn injected into the
#' instrument, in ul.
#'
#' @param data_precessing_software character indicates the software the data
#' processing was done.
#'
#' @export
#' @author Chenghao Zhu
MetabolomicsExperimentData = function(
    institute                = NULL,
    instrument               = NULL,
    resuspension_volumn_ul   = NULL,
    injection_volumn_ul      = NULL,
    data_processing_software = NULL
){
    new(Class = "MetabolomicsExperimentData",
        institute                = institute,
        instrument               = instrument,
        resuspension_volumn_ul   = resuspension_volumn_ul,
        injection_volumn_ul      = injection_volumn_ul,
        data_processing_software = data_processing_software)
}
################################################################################
#' @name MetabolomicsSet-class
#' @title S4 class to store metabolomics dataset
#'
#' @description
#' This is a S4 class inherits from the virtual
#' \code{\link{mSet-class}} to store the metabolomics datset. This class acts
#' as a data container for the metabolomics data. Once data is cleaned and
#' passed into the class, it is ready to do statistical analysis and
#' visualization.
#'
#' This class is not designed to handle mass spectrometry data and feature
#' annotation. This class should only be used after the raw MS data procesing.
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
#' @slot experiment_data A \code{\link{MetabolomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @exportClass MetabolomicsSet
#' @author Chenghao Zhu
setClass(Class = "MetabolomicsSet", contains = "mSet")
################################################################################
setClassUnion("MetabolomicsExperimentDataOrNULL",
              c("MetabolomicsExperimentData","NULL"))
################################################################################
setValidity(
    Class = "MetabolomicsSet",
    method = function(object){
        if(!isClass(object@experiment_data, "MetabolomicsExperimentDataOrNULL"))
            return("The experiment data must be an object of the MetabolomicsExperimentData class")
        callNextMethod(object)
    }
)
################################################################################
#' @name MetabolomicsSet
#' @title Construct a MetabolomicsSet object
#' @description This is the method to construct a
#' \code{\link{MetabolomicsSet-class}} object.
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
#' @param experiment_data A \code{\link{MetabolomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @export
#' @author Chenghao Zhu
MetabolomicsSet = function(
    conc_table      = NULL,
    sample_table    = NULL,
    feature_data    = NULL,
    experiment_data = NULL
){
    object = new("MetabolomicsSet",
                 conc_table = conc_table,
                 sample_table = sample_table,
                 feature_data = feature_data,
                 experiment_data = experiment_data)
    validObject(object)
    return(object)
}

















