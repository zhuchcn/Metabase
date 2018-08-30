################################################################################
#' @title S4 class to store the experiment data of a micrbiome experiment
#' @description This class defines the experiement_slot of the
#' \code{\link{MicrobiomeSet-class}}. This class inherits from the
#' \code{\link{ExperiemntData-class}}
#' @slot experiment_type character value indicates the type of experiemnt
#' (16S, 18S, etc.)
#' @slot processing_software character value indicates the software used for
#' data processing/otu clustering (DADA2, qiime2, etc)
#' @seealso \code{\link{MicorbiomeExperimentData}}
#' \code{\link{MicrobiomeSet-class}}
#' \code{\link{ExperimentData-class}}
#' @exportClass MicrobiomeExperimentData
#' @author Chenghao Zhu
setClass(
    "MicrobiomeExperimentData",
    representation = representation(
        experiment_type = "characterOrNULL",
        processing_software = "characterOrNULL"
    ),
    contains = "ExperimentData",
    prototype = prototype(
        experiment_type = NULL,
        processing_software = NULL
    )
)
################################################################################
#' @title Construct a MicrobiomeExperimentData object
#' @description The main constructor for the
#' \code{\link{MicrobiomeExperimentData-class}}
#' @param experiment_type character value indicates the type of experiemnt
#' (16S, 18S, etc.)
#' @param processing_software character value indicates the software used for
#' data processing/otu clustering (DADA2, qiime2, etc)
#' @seealso \code{\link{MicorbiomeExperimentData-class}}
#' \code{\link{MicrobiomeSet-class}}
#' \code{\link{ExperimentData-class}}
#' @export
#' @author Chenghao Zhu
MicrobiomeExperimentData = function(
    experiemnt_type = NULL,
    processing_software = NULL
){
    new("MicrobiomeExperimentData",
        experiment_type = experiemnt_type,
        processing_software = processing_software)
}
################################################################################
setClassUnion("MicrobiomeExperimentDataOrNull",
              c("MicrobiomeExperimentData", "NULL"))
################################################################################
#' @title S4 class to store Micorbiome Data
#' @description This class inherits from the \code{\link{mSet-class}} and stores
#' the microbiome data (16S seq etc.)
#'
#' @slot conc_table A \code{\link{conc_table-class}} object that stores the
#' concentration information from the experiment. The column names should be
#' the feature IDs, and the rownames should be the row IDs. This should be a
#' numeric matrix. This slot is equivalent to the OTU table.
#'
#' @slot sample_table A \code{\link{sample_table-class}} object that stores the
#' sample meta-data information. The row names should be sample IDs and
#' should match the column names of the conc_table.
#'
#' @slot feature_data A \code{\link{feature_data-class}} object that stores the
#' feature infromation during the experiment. The row names should be feature
#' IDs and should match the row names of the conc_table. The feature_data must
#' be all character and only stores the taxonomy inforamtion
#' (kingdom to species).
#'
#' @slot experiment_data A \code{\link{MetabolomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @seealso \code{\link{MicrobiomeSet}} \code{\link{mSet-class}}
#' \code{\link{MetabolomicsExperimentData-class}}
#'
#' @exportClass MicrobiomeSet
#' @author Chenghao Zhu
setClass(
    "MicrobiomeSet", contains = "mSet",
    validity = function(object){
        if(!isClass(object@experiment_data,
                    Class = "MicrobiomeExperimentDataOrNull"))
            return("[ Metabase ] [ MicrobiomeSet construction failure ] The experiment_data slot must be a MicrobiomeExperimentDat object")
        if(!is.null(object@feature_data)){
            if(any(sapply(object@feature_data, function(x) !is.character(x))))
                return("[ Metabase ] [ MicrobiomeSet construction failure ] The feature_data slot must be all character")
        }
    }
)
################################################################################
#' @title Main constructor for MicrobiomeSet class
#' @description  This is the main construction method to create a
#' \code{\link{MicrobiomeSet}} object.
#' @param conc_table A \code{\link{conc_table-class}} object that stores the
#' concentration information from the experiment. The column names should be
#' the feature IDs, and the rownames should be the row IDs. This should be a
#' numeric matrix. This slot is equivalent to the OTU table.
#'
#' @param sample_table A \code{\link{sample_table-class}} object that stores the
#' sample meta-data information. The row names should be sample IDs and
#' should match the column names of the conc_table.
#'
#' @param feature_data A \code{\link{feature_data-class}} object that stores the
#' feature infromation during the experiment. The row names should be feature
#' IDs and should match the row names of the conc_table. The feature_data must
#' be all character and only stores the taxonomy inforamtion
#' (kingdom to species).
#'
#' @param experiment_data A \code{\link{MetabolomicsExperimentData-class}}
#' object contains additional experiment information.
#'
#' @seealso \code{\link{MicrobiomeSet-class}} \code{\link{mSet-class}}
#' \code{\link{MetabolomicsExperimentData-class}}
#'
#' @export
#' @author Chenghao Zhu
MicrobiomeSet = function(conc_table = NULL, sample_table = NULL,
                         feature_data = NULL, experiment_data = NULL){
    new("MicrobiomeSet",
        conc_table = conc_table, sample_table = sample_table,
        feature_data = feature_data, experiment_data = experiment_data)
}

################################################################################
#' @title convert a MicrobiomeSet object to phyloseq
#' @param mset a \code{\link{MicrobiomeSet-class}} object
#' @export
#' @author Chenghao Zhu
as_phyloseq = function(mset){
    if(!requireNamespace("phyloseq"))
        stop("[ Metabase: PcakageNotFound ] Package phyloseq not installed.",
             call. = FALSE)

    if(!isClass(mset, Class = "MicrobiomeSet"))
        stop("[ Metabase ] The input object must be a MicrobiomeSet class.",
             call. = FALSE)

    otu_table = mset@conc_table %>%
        as("matrix") %>%
        phyloseq::otu_table(taxa_are_rows = TRUE)

    if(is.null(mset@sample_table)){
        sam_data = NULL
    }else{
        sam_data = mset@sample_table %>%
            as("data.frame") %>%
            phyloseq::sample_data()
    }

    if(is.null(mset@feature_data)){
        tax_table = NULL
    } else {
        tax_table = mset@feature_data %>%
            as("data.frame") %>%
            as("matrix") %>%
            phyloseq::tax_table()
    }

    phyloseq::phyloseq(otu_table, sam_data, tax_table)
}
################################################################################
#' @export
setMethod(
    "show", signature = "MicrobiomeSet",
    definition = function(object){
        cat(str_pad(">>>>>> Microbiome Experiment <<<<<<", 55, "left", " "))
        cat("\n")
        callNextMethod()
    }
)
