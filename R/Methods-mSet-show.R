################################################################################
#' @keywords internal
'%+%' = function(a, b) paste0(a, b)
################################################################################
#' @import stringr
#' @export
setMethod(
    "show", signature = "conc_table",
    definition = function(object){
        n.row = nrow(object)
        n.col = ncol(object)

        row.show = min(nrow(object), 8)
        col.show = min(ncol(object), 6)

        cat(str_pad(">>>>>> Concentration Table <<<<<<", 50, "left", " ") %+%
            str_pad("\nconc_table()", 35, "right", " ") %+%
            "[ " %+%
            str_pad(n.row, 3, "left", " ") %+%
            " features and " %+%
            str_pad(n.col, 3, "left", " ") %+%
            " samples ]\n\n" %+%
            "Showing the first " %+%
            row.show %+% " samples and " %+%
            col.show %+% " features...\n\n")

        show(object[1:row.show, 1:col.show])

        cat("\n")
        cat("... " %+% str_pad(n.col-col.show, 3, "left", " ") %+%
            " more samples\n")
        cat("... " %+% str_pad(n.row-row.show, 3, "left", " ") %+%
            " more features\n")
    }
)
################################################################################
#' @export
setMethod(
    "show", signature = "sample_table",
    definition = function(object){
        n.row = nrow(object)
        n.col = ncol(object)

        row.show = min(nrow(object), 8)
        col.show = min(ncol(object), 6)

        cat(str_pad(">>>>>> Sample Table <<<<<<", 45, "left", " ") %+%
            str_pad("\nsample_table()", 28, "right", " ") %+%
            "[ " %+%
            str_pad(n.row, 3, "left", " ") %+%
            " samples and " %+%
            str_pad(n.col, 3, "left", " ") %+%
            " sample variables ]\n\n" %+%
            "Showing the first " %+%
            col.show %+% " samples and " %+%
            row.show %+% " sample variables...\n\n")

        show(object[1:row.show, 1:col.show])

        cat("\n")
        cat("... " %+% str_pad(n.row-row.show, 3, "left", " ") %+%
                " more samples\n")
        cat("... " %+% str_pad(n.col-col.show, 3, "left", " ") %+%
                " more sample variables\n")
    }
)
################################################################################
#' @export
setMethod(
    "show", signature = "feature_data",
    definition = function(object){
        n.row = nrow(object)
        n.col = ncol(object)

        row.show = min(nrow(object), 8)
        col.show = min(ncol(object), 6)

        cat(str_pad(">>>>>> Feature Data <<<<<<", 48, "left", " ") %+%
            str_pad("\nfeature_data()", 24, "right", " ") %+%
            "[ " %+%
            str_pad(n.row, 3, "left", " ") %+%
            " features and " %+%
            str_pad(n.col, 3, "left", " ") %+%
            " feature variables ]\n\n" %+%
            "Showing the first " %+%
            row.show %+% " samples and " %+%
            col.show %+% " features...\n\n")

        show(object[1:row.show, 1:col.show])

        cat("\n")
        cat("... " %+% str_pad(n.col-col.show, 3, "left", " ") %+%
                " more features\n")
        cat("... " %+% str_pad(n.row-row.show, 3, "left", " ") %+%
                " more feature variables\n")
    }
)
################################################################################
#' @export
setMethod(
    "show", signature = "MetabolomicsExperimentData",
    definition = function(object){
        cat(str_pad(">>>>>> Metabolomics Experiment Data <<<<<<", 51, "left", " "))
        cat("\n\n")
        slots = splat_object(object)
        for(i in 1:length(slots)){
            slot_name = gsub("_", " ", names(slots)[i])
            slot_name = str_to_title(slot_name)
            if(is.character(slots[[i]]) | is.numeric(slots[[i]])){
                cat(slot_name %+% ": " %+% slots[[i]])
                cat("\n")
            }else if(is.data.frame(slots[[i]])){
                cat(slot_name %+% ": a " %+% nrow(slots[[i]]) %+% " by " %+%
                        ncol(slots[[i]]) %+% " data frame")
            }
        }
    }
)
#' @export
setMethod(
    "show", signature = "LipidomicsExperimentData",
    definition = function(object){
        cat(str_pad(">>>>>> Lipidomics Experiment Data <<<<<<", 50, "left", " "))
        cat("\n\n")
        slots = splat_object(object)
        for(i in 1:length(slots)){
            slot_name = gsub("_", " ", names(slots)[i])
            slot_name = str_to_title(slot_name)
            if(is.character(slots[[i]]) | is.numeric(slots[[i]])){
                cat(slot_name %+% ": " %+% slots[[i]])
                cat("\n")
            }else if(is.data.frame(slots[[i]])){
                cat(slot_name %+% ": a " %+% nrow(slots[[i]]) %+% " by " %+%
                        ncol(slots[[i]]) %+% " data frame")
            }
        }
    }
)
################################################################################
#' @keywords internal
setMethod(
    "length", signature = "ExperimentData",
    definition = function(x){
        slots = splat_object(x)
        return(length(slots))
    }
)
################################################################################
#' @export
setMethod(
    "show", signature = "mSet",
    definition = function(object){

        # conc_table
        if(!is.null(object@conc_table)){
            cat(
                str_pad("conc_table():", 24, "right", " ") %+%
                "[ "  %+%
                str_pad(ncol(object@conc_table), 3, "left", " ") %+%
                " samples  and " %+%
                str_pad(nrow(object@conc_table), 3, "left", " ") %+%
                " features          ]\n"
            )
        }

        # sample_table
        # conc_table
        if(!is.null(object@sample_table)){
            cat(
                str_pad("sample_table():", 24, "right", " ") %+%
                "[ "  %+%
                str_pad(nrow(object@sample_table), 3, "left", " ") %+%
                " samples  and " %+%
                str_pad(ncol(object@sample_table), 3, "left", " ") %+%
                " sample  variables ]\n"
            )
        }

        # feature_data
        if(!is.null(object@feature_data)){
            cat(
                str_pad("feature_data():", 24, "right", " ") %+%
                "[ "  %+%
                str_pad(nrow(object@feature_data), 3, "left", " ") %+%
                " features and " %+%
                str_pad(ncol(object@feature_data), 3, "left", " ") %+%
                " feature variables ]\n"
            )
        }

        # experiment_data
        if(!is.null(object@experiment_data)){
            cat(
                str_pad("experiment_data():", 24, "right", " ") %+%
                "[ " %+%
                str_pad(length(object@experiment_data), 3, "left", " ") %+%
                " experiment attributes              ]\n"
            )
        }
    }
)
################################################################################
#' @export
setMethod(
    "show", signature = "MetabolomicsSet",
    definition = function(object){
        cat(str_pad(">>>>>> Metabolomics Experiment <<<<<<", 50, "left", " "))
        cat("\n")
        callNextMethod()
    }
)

