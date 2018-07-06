################################################################################
setGeneric("conc_table", function(object) standardGeneric("conc_table"))
setGeneric("sample_table", function(object) standardGeneric("sample_table"))
setGeneric("feature_data", function(object) standardGeneric("feature_data"))
setGeneric("experiment_data", function(object) standardGeneric("experiment_data"))
setGeneric("design_data", function(object) standardGeneric("design_data"))
setGeneric("conc_table<-",
           function(object, value) standardGeneric("conc_table<-"))
setGeneric("sample_table<-",
           function(object, value) standardGeneric("sample_table<-"))
setGeneric("feature_data<-",
           function(object, value) standardGeneric("feature_data<-"))
setGeneric("experiment_data<-",
           function(object, value) standardGeneric("experiment_data<-"))
setGeneric("design_data<-",
           function(object, value) standardGeneric("design_data<-"))
################################################################################
#' @rdname conc_table
#' @export
setMethod(
    "conc_table", signature = "mSet",
    definition = function(object){
        return(object@conc_table)
    }
)
################################################################################
#' @name conc_table<-
#' @title Assign a conc_table slot to a mSet object
#' @description This function assign a new \code{\link{conc_table-class}} to
#' the conc_table slot of a \code{\link{mSet-class}} object.
#' @param object A \code{\link{mSet-class}} object
#' @param value A \code{\link{conc_table-class}} object
#' @return a \code{\link{mSet-class}} object
#' @export
setReplaceMethod(
    "conc_table", signature = "mSet",
    definition = function(object, value){
        object@conc_table = value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname sample_table
#' @export
setMethod(
    "sample_table", signature = "mSet",
    definition = function(object){
        return(object@sample_table)
    }
)
################################################################################
#' @name sample_table<-
#' @title Assign a sample_table slot to a mSet object
#' @description This function assign a new \code{\link{sample_table-class}} to
#' the sample_table slot of a \code{\link{mSet-class}} object.
#' @param object A \code{\link{mSet-class}} object
#' @param value A \code{\link{sample_table-class}} object
#' @return a \code{\link{mSet-class}} object
#' @export
setReplaceMethod(
    "sample_table", signature = "mSet",
    definition = function(object, value){
        object@sample_table = value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @rdname feature_data
#' @export
setMethod(
    "feature_data", signature = "mSet",
    definition = function(object){
        return(object@feature_data)
    }
)
################################################################################
#' @name feature_data<-
#' @title Assign a feature_data slot to a mSet object
#' @description This function assign a new \code{\link{feature_data-class}} to
#' the feature_data slot of a \code{\link{mSet-class}} object.
#' @param object A \code{\link{mSet-class}} object
#' @param value A \code{\link{feature_data-class}} object
#' @return a \code{\link{mSet-class}} object
#' @export
setReplaceMethod(
    "feature_data", signature = "mSet",
    definition = function(object, value){
        object@feature_data = value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @name experiment_data
#' @title Access the experiment_data slot from a mSet object
#' @description
#' This is a accessor function for the experiment_data slot from a
#' \code{\link{mSet-class}} object.
#' @param object A \code{\link{mSet-class}} object
#' @return A \code{\link{ExperimentData-class}} object
#' @export
setMethod(
    "experiment_data", signature = "mSet",
    definition = function(object){
        return(object@experiment_data)
    }
)
################################################################################
#' @name experiment_data<-
#' @title Assign a experiment_data slot to a mSet object
#' @description This function assign a new \code{\link{experiment_data-class}} to
#' the experiment_data slot of a \code{\link{mSet-class}} object.
#' @param object A \code{\link{mSet-class}} object
#' @param value A \code{\link{experiment_data-class}} object
#' @return a \code{\link{mSet-class}} object
#' @export
setReplaceMethod(
    "experiment_data", signature = "mSet",
    definition = function(object, value){
        object@experiment_data = value
        validObject(object)
        return(object)
    }
)
################################################################################
#' @export
setAs(
    "ExperimentData",
    "list",
    function(from){
        return(splat_object(from))
    }
)
################################################################################

