################################################################################
#' @title Get dimemsions of the conc_table of a mSet object
#' @description Get the dimensions of the conc_table of a mSet object
#' @seealso \code{\link{mSet-class}}, \code{\link{conc_table-class}},
#' \code{\link{MetabolomicsSet}}
#' @param x A \code{\link{mSet-class}} or derived class object.
#' @return A integer vector with length of 2, with the first indicates the
#' number of features, and the second number of samples
#' @export
#setGeneric("dim", function(x) standardGeneric("dim"))
setMethod(
    "dim", signature = "mSet",
    function(x){
        return(dim(x@conc_table))
    }
)
################################################################################
setGeneric("nsamples", function(x) standardGeneric("nsamples"))
#' @title Get number of samples from a mSet object
#' @description Get the number of samples from a given \code{\link{mSet-class}}
#' or derived classes.
#' @param x A \code{\link{mSet-class}} or derived class object
#' @return A integer value
#' @seealso \code{\link{mSet-class}}, \code{\link{conc_table-class}},
#' \code{\link{sample_table-class}}, \code{\link{MetabolomicsSet-class}}
#' @export
setMethod(
    "nsamples", signature = "mSet",
    function(x){
        return(ncol(x@conc_table))
    }
)
################################################################################
setGeneric("nfeatures", function(x) standardGeneric("nfeatures"))
#' @title Get number of features from a mSet object
#' @description Get the number of features from a given \code{\link{mSet-class}}
#' or derived classes.
#' @param x A \code{\link{mSet-class}} or derived class object
#' @return A integer value
#' @seealso \code{\link{mSet-class}}, \code{\link{conc_table-class}},
#' \code{\link{feature_data-class}}, \code{\link{MetabolomicsSet-class}}
#' @export
setMethod(
    "nfeatures", signature = "mSet",
    function(x){
        return(nrow(x@conc_table))
    }
)
################################################################################
setGeneric("sampleNames", function(x) standardGeneric("sampleNames"))
#' @title Get the sample names of a mSet object
#' @description Get the sample names from a given \code{\link{mSet-class}}
#' or derived classes.
#' @param x A \code{\link{mSet-class}} or derived class object
#' @return A character vector.
#' @seealso \code{\link{mSet-class}}, \code{\link{conc_table-class}},
#' \code{\link{sample_table-class}}, \code{\link{MetabolomicsSet-class}}
#' @export
setMethod(
    "sampleNames", signature = "mSet",
    function(x){
        return(colnames(x@conc_table))
    }
)
################################################################################
setGeneric("featureNames", function(x) standardGeneric("featureNames"))
#' @title Get the feature names of a mSet object
#' @description Get the feature names from a given \code{\link{mSet-class}}
#' or derived classes.
#' @param x A \code{\link{mSet-class}} or derived class object
#' @return A character vector.
#' @seealso \code{\link{mSet-class}}, \code{\link{conc_table-class}},
#' \code{\link{feature_data-class}}, \code{\link{MetabolomicsSet-class}}
#' @export
setMethod(
    "featureNames", signature = "mSet",
    function(x){
        return(rownames(x@conc_table))
    }
)
################################################################################
setGeneric("subset_samples",
           function(x, samples) standardGeneric("subset_samples"))
setGeneric("subset_features",
           function(x, features) standardGeneric("subset_features"))
################################################################################
#' @rdname subset_samples-mSet-method
#' @title Subset an mSet object
#' @description The subset_samples() and subset_features() functions gets a
#' subset of an mSet object either by samples or features. The two functions
#' can either take character vectors of sample/feature names, logical values,
#' or integer indices to specify the samples or features to subset.
#' @aliases subset_samples
#' @param x An \code{\link{mSet-class}} or derived object
#' @param samples The samples to subset. Can be character, logical, or integers.
#' @return An \code{\linke{mSset-class}} or derived object
#' @export
setMethod(
    "subset_samples", signature = c("mSet", "character"),
    function(x, samples){
        x@conc_table = conc_table(x@conc_table[,samples])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples,])
        validObject(x)
        return(x)
    }
)
#' @rdname subset_samples-mSet-method
#' @aliases subset_samples
#' @export
setMethod(
    "subset_samples", signature = c("mSet", "logical"),
    function(x, samples){
        x@conc_table = conc_table(x@conc_table[,samples])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples,])
        validObject(x)
        return(x)
    }
)
#' @rdname subset_samples-mSet-method
#' @aliases subset_samples
#' @export
setMethod(
    "subset_samples", signature = c("mSet", "integer"),
    function(x, samples){
        x@conc_table = conc_table(x@conc_table[,samples])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples,])
        validObject(x)
        return(x)
    }
)
################################################################################
#' @rdname subset_samples-mSet-method
#' @aliases subset_features
#' @param features The features to subset. Can be character, logical, or
#' integers.
#' @export
setMethod(
    "subset_features", signature = c("mSet", "character"),
    function(x, features){
        x@conc_table = conc_table(x@conc_table[features,])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,])
        validObject(x)
        return(x)
    }
)
#' @rdname subset_samples-mSet-method
#' @aliases subset_features
#' @export
setMethod(
    "subset_features", signature = c("mSet", "logical"),
    function(x, features){
        x@conc_table = conc_table(x@conc_table[features,])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,])
        validObject(x)
        return(x)
    }
)
#' @rdname subset_samples-mSet-method
#' @aliases subset_features
#' @export
setMethod(
    "subset_features", signature = c("mSet", "integer"),
    function(x, features){
        x@conc_table = conc_table(x@conc_table[features,])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,])
        validObject(x)
        return(x)
    }
)
################################################################################
#' @title Transform an mSet object by samples
#' @description Transform the conc_table slot of an \code{\link{mSet-class}}
#' object by samples.
#' @param object An \code{\link{mSet-class}} or derived class object.
#' @param fun A function to apply.
#' @param ... Arguments to pass to the function/
#' @return An \code{\link{mSet-class}} or derived class object
#' @export
#' @seealso \code{\link{mSet-class}}, \code{\link{transform_by_features}}
transform_by_samples = function(object, fun, ...){

    if(length(fun(1:10)) != 10) stop("Function invalid")

    conc_table = apply(conc_table(object), 2, fun, ...)

    rownames(conc_table) = featureNames(object)
    colnames(conc_table) = sampleNames(object)

    object@conc_table = conc_table(conc_table)
    validObject(object)
    return(object)
}
################################################################################
#' @title Transform an mSet object by features
#' @description Transform the conc_table slot of an \code{\link{mSet-class}}
#' object by features.
#' @param object An \code{\link{mSet-class}} or derived class object.
#' @param fun A function to apply.
#' @param ... Arguments to pass to the function/
#' @return An \code{\link{mSet-class}} or derived class object
#' @export
#' @seealso \code{\link{mSet-class}}, \code{\link{transform_by_features}}
transform_by_features = function(object, fun, ...){

    if(length(fun(1:10)) != 10) stop("Function invalid")

    conc_table = t(apply(conc_table(object), 1, fun, ...))

    rownames(conc_table) = featureNames(object)
    colnames(conc_table) = sampleNames(object)

    object@conc_table = conc_table(conc_table)
    validObject(object)
    return(object)
}



