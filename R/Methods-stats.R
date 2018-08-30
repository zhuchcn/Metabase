################################################################################
#' @title Run linear model with the limma package
#' @description This is a function that fits linear models for each single
#' features in a \code{\link{mSet-class}} object. It calls the function
#' \code{\link{lmFit}}, followed by \code{\link{eBayes}} for t tests.
#' The result is returned by \code{\link{topTable}}. A model matrix is
#' requred to be passed to the lmFit function.
#'
#' @param object A \code{\link{mSet-class}} object.
#' @param design A design matrix. Can be created using the
#' \code{\link{model.matrix}} funciton.
#' @param transform A transform function to apply. The defaul is log2
#' transformation.
#' @param coef The number of coefficient to return by topTable.
#' See \code{\link{topTable}}.
#' @param p.value Should be the same as coef.
#'
#' @return A data.frame. See \code{\link{topTable}}
#' @export
#' @author Chenghao Zhu
mSet_limma = function(object, design, transform = function(x){log2(x+1)},
                      coef, p.value){
    if(!requireNamespace("limma", quietly = TRUE))
        stop("[ Metabase: PackageNotFound ] The package limma is not found.")
    if(!inherits(object, "mSet"))
        stop("[ Metabase ] object must inherit from mSet", call. = FALSE)

    data = object@conc_table
    data = transform(data)

    fit = limma::lmFit(data, design)
    fit_ebayes = limma::eBayes(fit)
    res = limma::topTable(fit_ebayes, coef = coef, p.value = p.value,
                          number = nfeatures(object), sort.by = "none")

    res = res[,c("AveExpr", "logFC", "t", "P.Value", "adj.P.Val")]
    names(res) = c("baseMean", "logFC", "stat", "pvalue", "padj")

    return(res)
}
################################################################################
#' @title Run negative bionomial bistribution based differential expression
#' analysis with the DESeq2 package
#' @description Run differential expression analysis base on negative binomial
#' distribution using the DESeq2 pacakge. The DESeq2 pacakge must be installed.
#' See \code{\link{DESeq}} for more detail
#' @param object \code{\link{mSet-class}} object. The conc_table must be count
#' data.
#' @param design A design matrix. Can be created using the
#' \code{\link{model.matrix}} funciton.
#' @param result integer or string indicates the coefficient to build the
#' results table.
#' @export
#' @author Chenghao Zhu
#' @seealso \code{\link{DESeq}}
mSet_deseq = function(object, design, result){
    if(!requireNamespace("DESeq2", quietly = TRUE))
        stop("[ Metabase: PackageNotFound ] The package DESeq2 is not found.")
    if(!inherits(object, "mSet"))
        stop("[ Metabase ] object must inherit from mSet", call. = FALSE)

    edata = object@conc_table %>% as.matrix
    pdata = object@sample_table %>% as.data.frame

    ds = DESeq2::DESeqDataSetFromMatrix(edata, pdata, design)
    ds = DESeq2::DESeq(ds)
    if(is.numeric(result)){
        result = as.integer(result)
        result = DESeq2::resultsNames(ds)[result]
    } else {
        result = gsub(
            "[\\;\\:\\,\\<\\>\\?\\/\\'\\}\\{\\+\\=\\-\\`\\~]\\@\\#\\$\\%\\^\\&\\*",
            "\\.", result)
    }
    res = DESeq2::results(ds, name = result) %>% as.data.frame()
    res = res[,c("baseMean", "log2FoldChange", "stat", "pvalue", "padj")]
    names(res) = c("baseMean", "logFC", "stat", "pvalue", "padj")
    return(res)
}
