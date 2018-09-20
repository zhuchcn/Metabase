#' @name MultiExperimentData
#' @title S4 class to store experiment data for MultxSet
#' @description This is the S4 class for stroing experiment data for the
#' \code{\link{MultxSet-class}} object.
#' @seealso \code{\link{MultxSet-class}}
#' @exportClass MultiExperimentData
#' @author Chenghao Zhu
setClass(
    Class = "MultiExperimentData",
    representation = representation(
        experiment_type = "characterOrNULL"
    ),
    contains = "ExperimentData",
    prototype = prototype(
        experiment_type = NULL
    )
)

#' @title Construct a MultiExperimentData object
#' @description The constructor for \code{\link{MultiExperimentData-class}}.
#' See the MultiExperimentData-class for more detail.
#' @param experiment_type A character string indicates the type of experiement.
#' @export
#' @author Chenghao Zhu
MultiExperimentData = function(experiment_type){
    new("MultiExperimentData",
        experiment_type = experiment_type)
}

#' @name MultxSet-class
#' @title S4 class for mutiple experiment types
#' @description The MultxSet class is a flexible data container for different
#' type of experiment data, such as clinical values, dietary data, or any
#' biochemical assies. Now it simply inherits from the virtual
#' \code{\link{mSet-class}} without any modification or restriction. For more
#' detail, please see the mSet help document.
#' @seealso \code{\link{mSet-class}}, \code{\link{MultxSet}}
#' @author Chenghao Zhu
#' @exportClass MultxSet
setClass(
    Class = "MultxSet", contains = "mSet",
    validity = function(object){
        if(!is.null(object@experiment_data) & class(object@experiment_data) != "MultiExperimentData")
            return("The experiment_data must be a MultiExperimentData object")
    }
)


#' @title Construct a MultxSet class object
#' @description The main constructor to creat a \code{\link{MultxSet-class}}
#' object.
#' @param conc_table a \code{\link{conc_table-class}} object
#' @param sample_table a \code{\link{sample_table-class}} object
#' @param feature_data a \code{\link{feature_data-class}} object
#' @param experiment_data a \code{\link{experiment_data-class}} object. The
#' experiment_data slot in MultxSet is not defined so it can not be used yet.
#'
#' @seealso \code{\link{MultxSet-class}}, \code{\link{mSet-class}}
#' @author Chenghao Zhu
#' @export
MultxSet = function(conc_table      = NULL,
                    sample_table    = NULL,
                    feature_data    = NULL,
                    experiment_data = NULL){
    new(Class = "MultxSet",
        conc_table      = conc_table,
        sample_table    = sample_table,
        feature_data    = feature_data,
        experiment_data = experiment_data)
}

#' @export
setMethod(
    "show", signature = "MultxSet",
    definition = function(object){
        header = ">>>>>>>> " %+% object@experiment_data$experiment_type %+% " <<<<<<<<"
        cat(str_pad(header, width = 50, side = "left", pad = " "))
        cat("\n")
        callNextMethod()
    }
)
