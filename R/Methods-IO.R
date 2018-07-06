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
import_wcmc_excel_raw = function(file,
                                 sheet,
                                 conc_range,
                                 sample_range,
                                 feature_range){
    if(!requireNamespace("readxl"))
        stop("The package 'readxl' is required for this funciton. Please install it.")
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
        as.data.frame %>%
        column_to_rownames("Label") %>%
        t %>% as.data.frame
    # read feature_data
    feature_data = readxl::read_excel(
        path      = file,
        sheet     = sheet,
        range     = feature_range,
        col_names = TRUE,
        na        = ""
    ) %>% as.data.frame

    # assign names
    rownames(feature_data) = "Feature" %+%
        str_pad(1:nrow(feature_data), side = "left",
                width = ceiling(log10(nrow(feature_data))), pad = "0")
    rownames(conc_table) = rownames(feature_data)
    colnames(conc_table) = rownames(sample_table)

    object = MetabolomicsSet(
        conc_table   = conc_table(conc_table),
        sample_table = sample_table(sample_table),
        feature_data = feature_data(feature_data)
    )
    return(object)
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
    qcs = object@conc_table[,qc_names]
    object@feature_data = as(object@feature_data, "data.frame") %>%
        rownames_to_column("feature_id") %>%
        mutate(
            qc_mean = rowMeans(qcs, na.rm = TRUE),
            qc_sd   = apply(qcs, 1, sd, na.rm = TRUE)
        ) %>%
        mutate(
            qc_cv = qc_mean / qc_sd
        ) %>%
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
        classes = c('CE','Cholesterol','CUDA','LPC','LPE','PC','PE','PG','SM',
                    "Cer", 'Sphingosine','Ceramide','DG','MG','MAG','TG','FA', "AC",
                    'GlcCer', 'ceramide')
        for(class in classes){
            if(grepl(class, x)){
                if(class == 'MAG')  return('MG')
                if(class == 'GlcCer') return('Cer')
                if(class == 'ceramide') return('Cer')
                if(class == "Ceramide") return("Cer")
                else  return(class)
            }
        }
        return(NA)
    }
    return(sapply(x, get_a_class))
}











