#' @title S4 class for mutiple experiment types
#' @description The MultiSet class is a flexible data container for different
#' type of experiment data, such as clinical values, dietary data, or any
#' biochemical assies. Now it simply inherits from the virtual
#' \code{\link{mSet-class}} without any modification or restriction. For more
#' detail, please see the mSet help document.
#' @seealso \code{\link{mSet-class}}, \code{\link{MultiSet}}
#' @author Chenghao Zhu
#' @exportClass MultiSet
MultiSet <- setClass(Class = "MultiSet", contains = "mSet")


#' @title Construct a MultiSet class object
#' @description The main constructor to creat a \code{\link{MultiSet-class}}
#' object.
#' @param conc_table a \code{\link{conc_table-class}} object
#' @param sample_table a \code{\link{sample_table-class}} object
#' @param feature_data a \code{\link{feature_data-class}} object
#' @param experiment_data a \code{\link{experiment_data-class}} object. The
#' experiment_data slot in MultiSet is not defined so it can not be used yet.
#'
#' @seealso \code{\link{MultiSet-class}}, \code{\link{mSet-class}}
#' @author Chenghao Zhu
#' @export
MultiSet = function(conc_table      = NULL,
                    sample_table    = NULL,
                    feature_data    = NULL,
                    experiment_data = NULL){
    new(Class = "MultiSet",
        conc_table      = conc_table,
        sample_table    = sample_table,
        feature_data    = feature_data,
        experiment_data = experiment_data)
}
