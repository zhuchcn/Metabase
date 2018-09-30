################################################################################
##########                    WCMC MetabolomicsSet                    ##########
################################################################################

################################################################################
#' @title Import West Coast Metabolomics Center data spreadsheet to a
#' MetabolomicsSet object
#' @description This funciton reads the West Coast Metabolomics Center style
#' data spreadsheet into a MetabolomicsSet object, by specifying the range of
#' the conc_table, sample_table, and feature_data.
#' @param file A character string to the path of the file.
#' @param sheet A character string indicates the name of the sheet.
#' @param conc_range A character string indicatss the cell range of
#' \code{\link{conc_table}}, like "B3:D87".
#' @param sample_range A character string indicates the cell range of the
#' \code{\link{sample_table-class}}.
#' @param feature_range A character string indicates the cell range of the
#' \code{\link{feature_data-class}}.
#' @export
#' @return A \code{\link{MetabolomcisSet-class}} object
#' @author Chenghao Zhu
import_wcmc_excel = function(file,
                             sheet,
                             conc_range,
                             sample_range,
                             feature_range,
                             InChIKey = NULL,
                             experiment_type = "metabolomics",
                             institute = NULL){
    if(!requireNamespace("readxl"))
        stop("The package 'readxl' is required for this funciton. Please install it.")

    if(is.null(institute))
        institute = "West Coast Metabolomics Center, CA"

    experiment_type = tolower(experiment_type)

    # read conc_table
    conc_table = readxl::read_excel(
        path      = file,
        sheet     = sheet,
        range     = conc_range,
        col_names = FALSE,
        na        = ""
    ) %>% as.data.frame %>% as.matrix
    # read sample_table
    sample_table = readxl::read_excel(
        path      = file,
        sheet     = sheet,
        range     = sample_range,
        col_names = TRUE,
        na        = ""
    ) %>%
        as.data.frame
    sample_table = column_to_rownames(sample_table,
                                      colnames(sample_table)[1]) %>%
        t %>% as.data.frame
    # read feature_data
    feature_data = readxl::read_excel(
        path      = file,
        sheet     = sheet,
        range     = feature_range,
        col_names = TRUE,
        na        = ""
    ) %>% as.data.frame
    if(!is.null(InChIKey))
        colnames(feature_data)[colnames(feature_data) == InChIKey] = "InChIKey"

    # assign names
    rownames(feature_data) = "Feature" %+%
        str_pad(1:nrow(feature_data), side = "left",
                width = ceiling(log10(nrow(feature_data))), pad = "0")
    rownames(conc_table) = rownames(feature_data)
    colnames(conc_table) = rownames(sample_table)

    # experiment_data = MetabolomicsExperimentData(
    #     institute = "West Coast Metabolomics Center"
    # )

    if(experiment_type == "lipidomics"){
        object = LipidomicsSet(
            conc_table   = conc_table(conc_table),
            sample_table = sample_table(sample_table),
            feature_data = feature_data(feature_data),
            experiment_data = LipidomicsExperimentData(
                institute = institute,
                experiment_type = experiment_type
            )
        )
    }else{
        object = MetabolomicsSet(
            conc_table   = conc_table(conc_table),
            sample_table = sample_table(sample_table),
            feature_data = feature_data(feature_data),
            experiment_data = MetabolomicsExperimentData(
                institute = institute,
                experiment_type = experiment_type
            )
        )
    }

    return(object)
}
################################################################################
#' @title Export mSet object into a Excle sheet
#'
#' @description This function allows users to export a mSet object to an Excle
#' spreadsheet. The output is a spreadsheet with feature data on the left,
#' sample table on the top and transposed, and the conc table in the middle.
#'
#' This function uses the xlsx pacakge to write the data into Excle format.
#' Please make sure the package is pre-installed before using it.
#'
#' @param object A \code{\link{mSet-class}} or derived object.
#' @param file A character string indicates the output path.
#'
#' @return A Excle spreadsheet
#' @export
#' @author Chenghao Zhu
export_excle = function(object, file){
    if(!requireNamespace(xlsx))
        stop("The package xlsx is required for this function. Please install it.",
             call. = FALSE)
    if(!inherits(object, "mSet"))
        stop("Only mSet or derived classes are supported", call. = FALSE)

    emt_mat = matrix("", ncol = ncol(object@feature_data),
                     nrow = ncol(object@sample_table)+1)
    featVar = c("feature_id", colnames(object@feature_data))
    sampVar = c("sample_id",  colnames(object@sample_table))
    emt_mat = rbind(emt_mat, featVar[-length(featVar)])
    emt_mat = cbind(emt_mat, c(sampVar, featVar[length(featVar)]))
    top_mat = cbind(emt_mat,
                    rbind(t(rownames_to_column(object@sample_table, "sample_id")),
                          "Concentration"))
    dimnames(top_mat) = NULL
    bot_mat = cbind(rownames_to_column(object@feature_data, "feature_id"),
                    object@conc_table) %>% as.matrix
    dimnames(bot_mat) = NULL
    data = rbind(top_mat, bot_mat)
    xlsx::write.xlsx(data, file = file, row.names = FALSE, col.names = FALSE,
               showNA = FALSE)
}

################################################################################
#' @title Collapse QC samples from a MetabolomcisSet object
#' @description Calculate the mean, standard deviation, and coefficient of
#' variance of the QC and put into the feature_data slot. The QC samples are
#' then removed from conc_table and sample_table.
#' @param object A \code{\link{MetabolomicsSet-class}} object.
#' @param qc_names A character vector indicates the names of the QC samples.
#' Must be from the sampleNames of the object.
#' @export
#' @return A \code{\link{MetabolomcisSet-class}} object
#' @author Chenghao Zhu
collapse_QC = function(object, qc_names){
    if(!inherits(object, "mSet"))
        stop("Only mSet or derived classes are supported", call. = FALSE)

    options(warn = -1)
    qcs = object@conc_table[,qc_names]
    object@feature_data = as(object@feature_data, "data.frame") %>%
        rownames_to_column("feature_id") %>%
        mutate(
            qc_mean = rowMeans(qcs, na.rm = TRUE),
            qc_sd   = apply(qcs, 1, sd, na.rm = TRUE)
        ) %>%
        mutate(qc_cv = qc_mean / qc_sd) %>%
        column_to_rownames("feature_id") %>%
        feature_data()
    object = subset_samples(object, !sampleNames(object) %in% qc_names)
    return(object)
}
################################################################################
#' @title Assign lipid class based on lipid annotation name
#' @description This function assign lipid class to each lipid feature
#' according to their annotation names. The annotations need to be in a
#' LipidBlast style. The lipid class returned is also in a LipidBlast style.
#' @param x Character vector of the annotated lipid feature names. Must be in
#' a LipidBlast style
#' @return Character vector fo lipid class.
#' @export
#' @author Chenghao Zhu
assign_lipid_class = function(x){
    get_a_class = function(x){
        classes = c('CE', 'Cholesteryl ester','Cholesterol','CUDA','LPC','LPE',
                    'PC','PE','PG', "PI", "PA", 'SM', "Cer", 'Sphingosine','Ceramide','DG',
                    'MG','MAG','TAG', "TG",'FA', "AC", 'GlcCer', 'ceramide',
                    'Acylcarnitine')
        for(class in classes){
            if(grepl(class, x)){
                if(class == "Cholesteryl ester") return("CE")
                if(class == 'MAG')  return('MG')
                if(class == "TAG") return("TG")
                if(class == 'GlcCer') return('Cer')
                if(class == 'ceramide') return('Cer')
                if(class == "Ceramide") return("Cer")
                if(class == "Acylcarnitine") return("AC")
                else  return(class)
            }
        }
        return(NA)
    }
    return(sapply(x, get_a_class))
}
################################################################################
#' @title Calculate concentration by calibrating from internal standards
#' @description Generic function for the \code{\link{LipidomcisSet-class}}
#' object to calculate the concentration of each feature. The object must have
#' a valid internal-standard slot in the experiment_data. It must also have
#' the sample_volumn in ul in the experiment_data (slot name: sample_volumn_ul).
#' @details
#' The concentration of each feature is calculated as:
#' \deqn{Conc_{i,j} = \frac{Intensity_{i,j}}{Intensity_{i,k}} \times \frac{Spiked_{k}}{Sample Vol_{i}}}
#' Where the \eqn{Conc_{i,j}} is the calculated concentration of feature j in
#' samplei. The \eqn{Intensity_{i,j}} is the raw MS intensity of feature j in
#' samplei. The \eqn{Intensity_{i,k}} is the raw MS intensity of standard k,
#' which has the same class as feature j, in sample i. The \eqn{Spiked_{k}} is
#' the spiked amount in sample k. And the \eqn{Sample Vol_{i}} is the sample
#' volumn used in the beginning of processing.
#'
#' The returned concentration is in the unit of ug/ml
#'
#' @param object a \code{\link{LipiomicsSet-class}} object
#' @return a \code{\link{LipidomicsSet-class}} object
#' @export
#' @author Chenghao Zhu
#'
calibrate_lipidomics_wcmc = function(object, class, cid, ESI){
    if(!isClass(object, Class = "LipidomicsSet"))
        stop("This function only supports LipidomicsSet data.", call. = FALSE)
    if(is.null(object@experiment_data@internal_standards))
        stop("The experiment_data must contain a `internal_standards` slot",
             call. = FALSE)
    if(is.null(object@experiment_data@sample_volumn_ul))
        stop("The experiment_data slot must contain `sample_volumn_ul`",
             call. = FALSE)
    if(missing(class))
        stop("Must have 'class' that specifies the feature variable of lipid class",
             call. = FALSE)
    if(!class %in% colnames(object@feature_data))
        stop("The class variable '" %+% class %+% "' not found in the feature_data",
             call. = FALSE)
    if(missing(cid))
        stop("Must have 'cid' that specifies the compound ID for each feature such as InChIKey")
    if(!cid %in% colnames(object@feature_data))
        stop("The cid variable '" %+% cid %+% "' not found in the feature_data",
             call. = FALSE)
    if(missing(ESI))
        stop("Must have 'ESI' that specifies the feature variable of ESI mode")
    if(!ESI %in% colnames(object@feature_data))
        stop("The ESI variable '" %+% ESI %+% "' not found in the feature_data",
             call. = FALSE)

    is_df = object@experiment_data@internal_standards

    object_clean = subset_features(
        object,
        !object@feature_data[, cid] %in% is_df[,cid]
    )
    object_istd = subset_features(
        object,
        object@feature_data[,cid] %in% is_df[,cid]
    )
    sample_vol = object@experiment_data@sample_volumn_ul
    conc_table = sapply(1:nfeatures(object_clean), function(i){
        int = object_clean@conc_table[i,]
        lipid.class = object_clean@feature_data[i, class]
        if(is.na(lipid.class))
            stop("Unknown lipid class. Feature ID: " %+% featureNames(object_clean)[i],
                 call. = FALSE)
        ESI.mode = object_clean@feature_data[i, ESI]
        istd.cid = is_df[is_df[,class] == lipid.class, cid]
        int.is = object_istd@conc_table[object_istd@feature_data[,ESI] == ESI.mode &
                                            object_istd@feature_data[,cid] == istd.cid]
        spike.amt = is_df$spike_amt[is_df[,cid] == istd.cid]
        return(int/int.is * spike.amt / sample_vol * 1000)
    }) %>% t
    rownames(conc_table) = featureNames(object_clean)
    object_clean@conc_table = conc_table(conc_table)
    object_clean@experiment_data@conc_table_unit = "ug/ml"
    return(object_clean)
}

################################################################################
#' @title Filter features by QC CV
#' @description This function deals with the situation where features are
#' detetcted in both positive and negative ESI mode. The qc_cv is used as a
#' reference and the one with a lower cv is kept, and the other one is dropped.
#' @param object A \code{\link{MetabolomicsSet-class}} object
#' @param cv A character string, indicates the column name of feature data,
#' that has the coefficient of variance for each feature.
#' @param cid A character string, indicates the column name of feature data,
#' that has the chemical identifier for each feature. The chemical identifier
#' must be unique to each feature. Some standard chemical identifier are PubMed
#' ID, Lipidmaps ID, etc.
#' @return A \code{\link{MetabolomicsSet-class}} object
#' @export
#' @author Chenghao Zhu
filter_by_cv = function(object, cv, cid){
    if(!inherits(object, "MetabolomicsSet"))
        stop("The function only works for MetabolomicsSet data",
             call. = FALSE)
    if(!cv %in% colnames(object@feature_data))
        stop("The argument cv not found in feature data",
             call. = FALSE)
    if(!cid %in% colnames(object@feature_data))
        stop("The argument cid not found in feature data",
             call. = FALSE)
    options(warn = -1)
    feature_data(object) = feature_data(object) %>%
        rownames_to_column("feature_id") %>%
        group_by(!!sym(cid)) %>%
        mutate(keep = eval(parse(text = paste0(cv, "== min(", cv, ")")))) %>%
        ungroup %>%
        as.data.frame() %>%
        column_to_rownames("feature_id") %>%
        feature_data()

    object = subset_features(object, feature_data(object)$keep)
    return(object)
}
