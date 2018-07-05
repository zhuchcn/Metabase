################################################################################
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
