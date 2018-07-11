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
        stop("The package limma is required to run this funciton. Please install it.")
    if(!inherits(object, "mSet")) stop("object must be mSet", call. = FALSE)

    data = object@conc_table
    data = transform(data)

    fit = limma::lmFit(data, design)
    fit_ebayes = limma::eBayes(fit)
    top = limma::topTable(fit_ebayes, coef = coef, p.value = p.value,
                          number = nfeatures(object), sort.by = "none")

    return(top)
}
