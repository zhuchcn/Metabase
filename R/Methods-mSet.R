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
#' @rdname sampleNames-mSet-method
#' @title Get or set the sample names of a mSet object
#' @description Get or set the sample names from a given \code{\link{mSet-class}}
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
setGeneric("sampleNames<-", function(x, value) standardGeneric("sampleNames<-"))
#' @rdname sampleNames-mSet-method
#' @aliases sampleNames<-
#' @param value A character vector. The length must equal to the number of
#' samples of the object.
#' @export
setReplaceMethod(
    "sampleNames", signature = "mSet",
    function(x, value){
        colnames(x@conc_table) = value
        if(!is.null(x@sample_table))
            rownames(x@sample_table) = value
        validObject(x)
        return(x)
    }
)
################################################################################
setGeneric("featureNames", function(x) standardGeneric("featureNames"))
#' @rdname featureNames-mSet-method
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
setGeneric("featureNames<-", function(x, value) standardGeneric("featureNames<-"))
#' @rdname featureNames-mSet-method
#' @aliases featureNames<-
#' @param value A character vector. The length must equal to the number of
#' features of the object.
#' @export
setReplaceMethod(
    "featureNames", signature = "mSet",
    function(x, value){
        rownames(x@conc_table) = value
        if(!is.null(x@feature_data))
            rownames(x@feature_data) = value
        validObject(x)
        return(x)
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
#' @return An \code{\link{mSset-class}} or derived object
#' @export
setMethod(
    "subset_samples", signature = c("mSet", "character"),
    function(x, samples){
        x@conc_table = conc_table(x@conc_table[,samples, drop = FALSE])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples, , drop = FALSE])
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
        x@conc_table = conc_table(x@conc_table[,samples, drop = FALSE])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples,,drop = FALSE])
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
        x@conc_table = conc_table(x@conc_table[,samples, drop = FALSE])
        if(!is.null(x@sample_table))
            x@sample_table = sample_table(x@sample_table[samples, drop = FALSE])
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
        x@conc_table = conc_table(x@conc_table[features,,drop = FALSE])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,,drop = FALSE])
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
        x@conc_table = conc_table(x@conc_table[features,,drop = FALSE])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,,drop = FALSE])
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
        x@conc_table = conc_table(x@conc_table[features,,drop = FALSE])
        if(!is.null(x@feature_data))
            x@feature_data = feature_data(x@feature_data[features,,drop = FALSE])
        validObject(x)
        return(x)
    }
)
################################################################################
#' @title Transform an mSet object by sample
#' @description Transform the conc_table slot of an \code{\link{mSet-class}}
#' object, one sample at a time. This is similar to MARGIN = 2 in the
#' \code{\link{apply}} function.
#' @param object An \code{\link{mSet-class}} or derived class object.
#' @param fun A function to apply.
#' @param ... Arguments to pass to the function/
#' @return An \code{\link{mSet-class}} or derived class object
#' @export
#' @seealso \code{\link{mSet-class}}, \code{\link{transform_by_features}}
transform_by_sample = function(object, fun, ...){

    if(length(fun(1:nfeatures(object))) != nfeatures(object))
        stop("Function invalid")

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
#' object, one feature at a time. This is similar to MARGIN = 1 in the
#' \code{\link{apply}} function.
#' @param object An \code{\link{mSet-class}} or derived class object.
#' @param fun A function to apply.
#' @param ... Arguments to pass to the function/
#' @return An \code{\link{mSet-class}} or derived class object
#' @export
#' @seealso \code{\link{mSet-class}}, \code{\link{transform_by_features}}
transform_by_feature = function(object, fun, ...){

    if(length(fun(1:nsamples(object))) != nsamples(object))
        stop("Function invalid")

    conc_table = t(apply(conc_table(object), 1, fun, ...))

    rownames(conc_table) = featureNames(object)
    colnames(conc_table) = sampleNames(object)

    object@conc_table = conc_table(conc_table)
    validObject(object)
    return(object)
}
################################################################################
#' @title Summarize mSet by a feature variable
#' @description This function takes the conc_table of a mSet object, groups
#' features by a given feature variable, calculate the sum of each feature in
#' each group, and return a summarized mSet object.
#' @param object An \code{\link{mSet-class}} object
#' @param feature_var An character variable, must from the column names of the
#' feature_data slot. Length must be 1.
#' @return An \code{\link{mSet-class}} object
#' @export
#' @author Chenghao Zhu
#' @importFrom magrittr %>%
#' @import dplyr
#' @import reshape2
#' @import tibble
summarize_features = function(object, feature_var){
    if(!inherits(object, "mSet"))
        stop("The object does not inherit from mSet")
    if(!is.character(feature_var) | length(feature_var) != 1)
        stop("Invalid feature_var", call. = FALSE)
    if(!feature_var %in% colnames(object@feature_data))
        stop("The feature_var '" %+% feature_var %+% "' does not exist",
             call. = FALSE)

    object@conc_table = as.matrix(object@conc_table) %>%
        as.data.frame %>%
        mutate(feature_var = object@feature_data[,feature_var]) %>%
        melt(id.var = "feature_var",
             variable.name = "sample_id",
             value.name = "concentration") %>%
        group_by(feature_var, sample_id) %>%
        summarize(concentration = sum(concentration, na.rm = TRUE)) %>%
        dcast(feature_var~sample_id, value.var = "concentration") %>%
        column_to_rownames("feature_var") %>%
        as.matrix %>%
        conc_table()
    object@feature_data = NULL
    validObject(object)
    return(object)
}
################################################################################
#' @title Summarize mSet by sample variables
#' @description This function takes the conc_table of a mSet object, groups
#' samples by given feature variables, calculate the mean of each fsample in
#' each group, and return a summarized mSet object.
#' @param object An \code{\link{mSet-class}} object
#' @param sample_var An character variable, must from the column names of the
#' sample_data slot.
#' @return An \code{\link{mSet-class}} object
#' @export
#' @author Chenghao Zhu
summarize_samples = function(object, sample_var){

    if(!is.character(sample_var) |
       any(!sample_var %in% colnames(object@sample_table)))
        stop("Invalid sample_var", call. = FALSE)

    quo_sample_var = quos(!!!syms(sample_var))

    df = object@conc_table %>%
        t %>% as.data.frame %>%
        cbind(object@sample_table[sample_var]) %>%
        melt(id.var = sample_var,
             variable.name = "feature_id",
             value.name = "concentration") %>%
        group_by(!!!quo_sample_var, feature_id)  %>%
        summarize(concentration = mean(concentration, na.rm = TRUE)) %>%
        dcast(... ~ feature_id, value.var = "concentration")

    conc_table = df[, ! colnames(df) %in% sample_var ] %>% t
    colnames(conc_table) = 1:ncol(conc_table)
    conc_table = conc_table(conc_table)
    sample_table = sample_table(df[, sample_var])

    object@conc_table = conc_table
    object@sample_table = sample_table
    validObject(object)

    return(object)
}

