################################################################################
#' @name conc_table-class
#' @title S4 class for concentration table
#' @description
#' This S4 class stores the concentration table from a quantitative experiment.
#' Inherit from the matrix class. Samples should be in columns and features in
#' rows. The data should also have sample IDs as column names and feature IDs
#' as row names.
#' @seealso
#' \code{\link{conc_table}}
#' @author Chenghao Zhu
#' @exportClass conc_table
setClass(Class = "conc_table",contains = "matrix")
################################################################################
# validator
setValidity(
    Class = "conc_table",
    method = function(object){
        if(!is.numeric(object))
            return("Input data isn't numeric. Please verify")
        if(is.null(rownames(object)))
            return("Row names are required.")
        if(is.null(colnames(object)))
            return("Colum names are required.")
    }
)
################################################################################
#' @rdname conc_table
#' @title Construct or assess a conc_table object
#' @description
#' This is the method to either access the concentration table slot from a
#' \code{\link{mSet-class}} object, or construct a
#' \code{\link{conc_table-class}} object from a matrix. When use it as an
#' assessing method, a \code{\link{conc_table-class}} object
#' is returned. When use it as a construction method, a matrix is required.
#' @param object To construct a conc_table object, this is a numeric matrix of
#' the concentration table from a quantitative experiment. Samples should be in
#' columns and features in rows. The data should also have sample IDs as column
#' names and feature IDs as row names. To assess the conc_table slot from a
#' mSet object, this is a mSet object.
#' @return An \code{\link{conc_table-class}} object.
#' @export
#' @seealso
#' \code{\link{conc_table-class}}
#' @author Chenghao Zhu
conc_table = function(object){
    if(!is.matrix(object))
        stop("The conc_table mast be a matrix")
    object = new("conc_table", object)
    validObject(object)
    return(object)
}

################################################################################
#' @name sample_table-class
#' @title S4 class for sample metadata.
#' @description
#' S4 class for storing sample metadata information. Inherits from the
#' data.frame class. Rows should be samples and columns should be all the
#' variables. Sample IDs should be row names, and should match the column IDs
#' of the \code{\link{conc_table-class}}.
#' @exportClass sample_table
#' @seealso
#' \code{\link{sample_table}}
#' @author Chenghao Zhu
setClass(Class = "sample_table", contains = "data.frame")
################################################################################
#' @rdname sample_table
#' @title Construction or access a sample_table object
#' @description
#' This is the method to either access the sample table slot from a
#' \code{\link{mSet-class}} object, or construct a
#' \code{\link{sample_table-class}} object from a data frame. When use it as an
#' assessing method, a \code{\link{sample_table-class}} object
#' is returned. When use it as a construction method, a matrix is required.
#' @param object This is a data frame of the sample metadata from a quantitative
#'  experiment as a constructor. Samples should be in rows. And a mSet object
#' as an assessor.
#' @return A \code{\link{smaple_table-class}} object
#' @export
#' @seealso
#' \code{\link{sample_table-class}}
#' @author Chenghao Zhu
sample_table = function(object){
    object = new("sample_table", object)
    validObject(object)
    return(object)
}
################################################################################
# validator
setValidity(
    Class = "sample_table",
    method = function(object){
        if(is.null(colnames(object)))
            warning("Colum names are required")
    }
)
################################################################################
#' @name feature_data-class
#' @title S4 class for feature data.
#' @description
#' S4 class for storing the feature information from a quantitative experiment.
#' Inherits from the data.frame class. Rows should be features and columns
#' should be all the variables. Feature IDs should be row names, and should
#' match the row IDs of the \code{\link{conc_table-class}}.
#' @exportClass feature_data
#' @seealso
#' \code{\link{feature_data}}
#' @author Chenghao Zhu
setClass(Class = "feature_data", contains  = "data.frame")
################################################################################
#' @rdname feature_data
#' @title Construct or access a feature_data object
#' @description
#' This is the method to either access the feature data slot from a
#' \code{\link{mSet-class}} object, or construct a
#' \code{\link{feature-data-class}} object from a data frame. When use it as an
#' assessing method, a \code{\link{feature_data-class}} object
#' is returned. When use it as a construction method, a matrix is required.
#' @param object This is a data frame of the feature data from a quantitative
#' experiment to as a constructor. Features should be in rows. And a mSet
#' object as an assessor.
#' @return A \code{\link{feature_table-class}} object
#' @export
#' @seealso
#' \code{\link{feature_data-class}}
#' @author Chenghao Zhu
feature_data = function(object){
    object = new("feature_data", object)
    validObject(object)
    return(object)
}
################################################################################
# validator
setValidity(
    Class = "feature_data",
    method = function(object){
        if(is.null(colnames(object)))
            warning("Column names are required.")
    }
)

################################################################################
#' @name ExperimentData-class
#' @title A virtual S4 class for experiment data
#' @description
#' Quantitative experiments such as metabolomics and proteomics often have
#' additional information beside experiment result. This S4 class is to store
#' those information such as analysis institute, instrument, resuspension
#' volumn, and so on.
#'
#' This is a virtual class and can not be called directly, but only can be
#' inherited from.
#' @exportClass ExperimentData-class
#' @seealso
#' \code{\link{MetabolomicsExperimentData-class}}
#' @author Chenghao Zhu
setClass(
    Class = "ExperimentData",
    representation = representation(
        "VIRTUAL"
    )
)
################################################################################
setClassUnion("characterOrNULL",      c("NULL", "character"))
setClassUnion("numericOrNULL",        c("NULL", "numeric"))
setClassUnion("conc_tableOrNULL",     c("NULL", "conc_table"))
setClassUnion("sample_tableOrNULL",   c("NULL", "sample_table"))
setClassUnion("feature_dataOrNULL",   c("NULL", "feature_data"))
setClassUnion("ExperimentDataOrNULL", c("NULL", "ExperimentData"))
################################################################################
#' @name mSet-class
#' @title A virtual S4 class to store an quantitative experiment data.
#' @description
#' This is a virtual S4 class to store the entire dataset from a quantitative
#' experiment, such as metabolomics and proteomics experiments.
#'
#' This is a virtual class so it can only be inherited from, but not be
#' constructed directly. You can either use the classes the inherits this class
#' defined by this package, or you can define your own class and inherits it to
#' use some of its features.
#'
#' The classes that inherits from the virtual mSet class are: the
#' \code{\link{MetabolomicsSet-class}} and the \code{\link{ProteomicsSet-class}}
#' .
#'
#' The mSet class and all the classes that inherits from it should contain at
#' least the four slots that were discussed below.
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
#' @slot experiment_data This must be an object that inherits from the virtual
#' \code{\link{ExperimentData-class}}. This slot stores the experiment
#' information such as lab and instrument. It also varies among different
#' experiment types.
#'
#' @exportClass mSet
#' @author Chenghao Zhu
setClass(
    Class = "mSet",
    representation = representation(
        conc_table      = "conc_tableOrNULL",
        sample_table    = "sample_tableOrNULL",
        feature_data    = "feature_dataOrNULL",
        experiment_data = "ExperimentDataOrNULL",
        "VIRTUAL"
    ),
    prototype = prototype(
        conc_table      = NULL,
        sample_table    = NULL,
        feature_data    = NULL,
        experiment_data = NULL
    )
)
################################################################################
setValidity(
    "mSet",
    method = function(object){
        if(class(object@conc_table) == "NULL"){
            # require conc_table
            return("Please at least provide the conc_table.")
        }else{
            # if feature_data is present, needs to match with conc_table
            if(class(object@feature_data) != "NULL"){
                if((nrow(object@conc_table) != nrow(object@feature_data)) |
                   (all.equal(rownames(object@conc_table),
                              rownames(object@feature_data)) != TRUE )){
                    return("Column names in conc_table and feature_data don't match")
                }
            }
            # if sample_data is present, needs to match with conc_table
            if(class(object@sample_table) != "NULL"){
                if((ncol(object@conc_table) != nrow(object@sample_table)) |
                   (all.equal(colnames(object@conc_table),
                              rownames(object@sample_table)) != TRUE )){
                    return("The row names of conc_table don't match the column
                           names of the sample_table")
                }
            }
        }
    }
)
################################################################################
#' @keywords internal
splat_object = function(object){
    slot_names = slotNames(object)
    slots_list = lapply(slot_names, function(slot){
        eval(parse(text = paste0("object@", slot)))
    })
    names(slots_list) = slot_names
    return(slots_list)
}



