        ##%######################################################%##
        #                                                          #
        ####                  MetabolomicsSet                   ####
        #                                                          #
        ##%######################################################%##

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
#' @exportClass MetabolomicsExperimentData
#' @author Chenghao Zhu
setClass(
    Class = "MetabolomicsExperimentData",
    representation = representation(
        institute                = "characterOrNULL",
        experiment_type          = "characterOrNULL",
        instrument               = "characterOrNULL",
        resuspension_volumn_ul   = "numericOrNULL",
        injection_volumn_ul      = "numericOrNULL",
        data_processing_software = "characterOrNULL",
        sample_volumn_ul         = "numericOrNULL",
        internal_standards       = "dataframeOrNULL",
        conc_table_unit          = "characterOrNULL"
    ),
    contain = "ExperimentData",
    prototype = prototype(
        institute                = NULL,
        experiment_type          = NULL,
        instrument               = NULL,
        resuspension_volumn_ul   = NULL,
        injection_volumn_ul      = NULL,
        data_processing_software = NULL,
        sample_volumn_ul         = NULL,
        internal_standards       = NULL,
        conc_table_unit          = NULL
    )
)
################################################################################
setValidity(
    Class = "MetabolomicsExperimentData",
    function(object){
        if(!is.null(object@internal_standards)){
            if(is.null(object@internal_standards$InChIKey))
                stop("The 'internal_standards' slot must have a 'InChIKey' slot")
        }
    }
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
#' @param experiment_type character indicates what type of experiment this is,
#' for example: metabolomics or biogenic amines.
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
#' @param internal_standards data frame that contains the internal standards
#' spiked in to samples.
#'
#' @param conc_table_unit character, the conc_table value unit
#'
#' @export
#' @author Chenghao Zhu
MetabolomicsExperimentData = function(
    institute                = NULL,
    experiment_type          = NULL,
    instrument               = NULL,
    resuspension_volumn_ul   = NULL,
    injection_volumn_ul      = NULL,
    data_processing_software = NULL,
    sample_volumn_ul         = NULL,
    internal_standards       = NULL,
    conc_table_unit          = NULL
){
    new(Class = "MetabolomicsExperimentData",
        institute                = institute,
        experiment_type          = experiment_type,
        instrument               = instrument,
        resuspension_volumn_ul   = resuspension_volumn_ul,
        injection_volumn_ul      = injection_volumn_ul,
        data_processing_software = data_processing_software,
        sample_volumn_ul         = sample_volumn_ul,
        internal_standards       = internal_standards,
        conc_table_unit          = conc_table_unit)
}
################################################################################
#' @name MetabolomicsSet-class
#' @title S4 class to store metabolomics experiment data
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
        if(!isClass(object@experiment_data, Class = "MetabolomicsExperimentDataOrNULL"))
            return("The experiment data must be an object of the MetabolomicsExperimentData class")
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
    return(object)
}

        ##%######################################################%##
        #                                                          #
        ####                   LipidomicsSet                    ####
        #                                                          #
        ##%######################################################%##

################################################################################
#' @name LipidomicsExperimentData-class
#' @title S4 class to store experiment data for metabolomics experiment.
#' @description
#' This is a S4 class inherits from the
#' \code{\link{MetabolomicsExperimentData-class}}. It can be thought as a
#' special subclass of the MetabolomicsExperimentData, only for lipidomics
#' experiment. It's required for the \code{\link{LipidomicsSet-class}} object.
#'
#' @slot institute character indicates the institute where the experiment was
#' conducted
#'
#' @slot experiment_type character indicate what type of experiment it is, for
#' example, lipidomics.
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
#' @slot internal_standards a data frame objec. This slot is very strict. It
#' must have at list 4 columns: InChIKey, class, spike_amt, and spike_unit,
#' stores the InChIKey, class of the lipid, spiked amount, and the unit,
#' correspondingly. It also must have all the internal standards appear in the
#' conc_table slot.
#'
#' @slot conc_table_unit character, the conc_table value unit
#'
#' @seealso \code{\link{LipidomicsSet-class}},
#' \code{\link{MetabolomicsSet-class}},
#' \code{\link{MetabolomicsExperimentData-class}},
#' \code{\link{calibrate}}
#'
#' @exportClass LipidomicsExperimentData
#' @author Chenghao Zhu
setClass(
    Class = "LipidomicsExperimentData",
    contains = "MetabolomicsExperimentData",
    validity = function(object){
        if(!is.null(object@internal_standards)){
            if(is.null(object@internal_standards$InChIKey))
                return("LipidomicsExperimentData@internal_standard: must have InChIKey")
            if(is.null(object@internal_standards$class))
                return("LipidomicsExperimentData@internal_standard: must have lipid class")
            if(is.null(object@internal_standards$spike_amt))
                return("LipidomicsExperimentData@internal_standard: must have spike_amt")
            if(is.null(object@internal_standards$spike_unit))
                return("LipidomicsExperimentData@internal_standard: must have spike_unit")
            if(any(object@internal_standards$spike_unit != "ug") )
                return("LipidomicsExperimentData@internal_standard: unit must be in 'ug'")
        }
    }
)
################################################################################
#' @name LipidomicsExperimentData
#' @title Construction method for LipidomicsExperimentData-class
#' @description The construction method to build a
#' \code{\link{LipidomicsExperimentData-class}} object.
#'
#' @param institute character indicates the institute where the experiment was
#' conducted
#'
#' @param experiment_type character indicates what type of experiment this is,
#' for example: lipidomics or biogenic amines.
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
#' @param internal_standards a data frame objec. This slot is very strict. It
#' must have at list 4 columns: InChIKey, class, spike_amt, and spike_unit,
#' stores the InChIKey, class of the lipid, spiked amount, and the unit,
#' correspondingly. It also must have all the internal standards appear in the
#' conc_table slot.
#'
#' @param conc_table_unit character, the conc_table value unit
#'
#' @export
#' @author Chenghao Zhu
LipidomicsExperimentData = function(
    institute                = NULL,
    experiment_type          = NULL,
    instrument               = NULL,
    resuspension_volumn_ul   = NULL,
    injection_volumn_ul      = NULL,
    data_processing_software = NULL,
    sample_volumn_ul         = NULL,
    internal_standards       = NULL,
    conc_table_unit          = NULL
){
    new(Class = "LipidomicsExperimentData",
        institute                = institute,
        experiment_type          = experiment_type,
        instrument               = instrument,
        resuspension_volumn_ul   = resuspension_volumn_ul,
        injection_volumn_ul      = injection_volumn_ul,
        data_processing_software = data_processing_software,
        sample_volumn_ul         = sample_volumn_ul,
        internal_standards       = internal_standards,
        conc_table_unit          = conc_table_unit)
}
################################################################################
setClassUnion("LipidomicsExperimentDataOrNULL",
              c("LipidomicsExperimentData", "NULL"))
################################################################################
#' @name LipidomcisSet-class
#' @title S4 class to store lipidomics dataset
#'
#' @description
#' This is a S4 class inherits from the
#' \code{\link{MetabolomicsSet-class}} to store the lipidomics datset. This
#' class can be thought as a special case of the MetabolomicsSet. It acts
#' as a data container for the lipidomics data. Once data is cleaned and
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
#' @slot experiment_data A \code{\link{LipidomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @exportClass LipidomicsSet
#' @author Chenghao Zhu
setClass(
    Class = "LipidomicsSet",
    contains = "MetabolomicsSet",
    validity = function(object){
        if(!isClass(object@experiment_data, Class = "LipidomicsExperimentDataOrNULL"))
            return("LipidomicsSet@experiment_data: must be a LipidomicsExperimentData")
    }
)
################################################################################
#' @name LipidomicsSet
#' @title Construct a LipidomicsSet object
#' @description This is the method to construct a
#' \code{\link{LipidomicsSet-class}} object.
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
#' @param experiment_data A \code{\link{LipidomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @export
#' @author Chenghao Zhu
LipidomicsSet = function(
    conc_table      = NULL,
    sample_table    = NULL,
    feature_data    = NULL,
    experiment_data = NULL
){
    object = new("LipidomicsSet",
                 conc_table = conc_table,
                 sample_table = sample_table,
                 feature_data = feature_data,
                 experiment_data = experiment_data)
    return(object)
}
