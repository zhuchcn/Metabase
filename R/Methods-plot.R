################################################################################
#' @title Make a boxplot of a feature
#' @description
#' This function uses the ggplot2 package to generate a boxplot from a givin
#' \code{\link{mSet-class}} object by specifying a feature id. The feature id
#' must come from the feature names
#' @param object An \code{\link{mSet-class}} object
#' @param x A character string indicating the x variable. Must be one of the
#' sample data variables.
#' @param feature A character indicating the feature name to plot. Must be one
#' of the feature names.
#' @param rows A character string indicating the variable defines faceting
#' groups on columns. Must be one of the sample data variables.
#' @param cols A character string indicating the variable defines faceting
#' groups on columns. Must be one of the sample data variables.
#' @param line A character string indicating the variable used to draw lines
#' between points. Must be one of the sample data variables.
#' @param color A character string indicating the variable defines the color
#' of lines or points. Must be one of the sample data variables.
#' @param ... Other arguments see \code{\link{ggboxplot}}
#' @author Chenghao Zhu
#' @import ggplot2
#' @export
plot_boxplot = function(object, x, feature, rows = NULL, cols = NULL, line = NULL, color = NULL, ...){

    if(!requireNamespace("ggmetaplots")){
        stop("[ Metabase ] Please install the zheatmap:\n\n    devtools::install_github('zhuchcn/zheatmap')",
             call. = FALSE)
    }

    args = as.list(match.call())[-c(1:2)]
    args = Filter(Negate(is.null), args)

    # the devil match.call function returns symbols, unevaluated symbols if
    # variables are parsed (why do you do this R!!). The solution is to eval
    # the value in its parent environment.
    call.envir = parent.frame(1)
    args = lapply(args, function(arg){
        if(is.symbol(arg)){
            eval(arg, envir = call.envir)
        } else {
            arg
        }
    })

    # because that's what I did in ggmetaplots
    names(args)[names(args) == "feature"] = "y"

    sample_vars = unique(c(x, rows, cols, color, line))

    df = data.frame(
        Concentration = object@conc_table[feature,]
    ) %>%
        cbind(object@sample_table[,sample_vars])
    colnames(df)[-1] = sample_vars

    args$data = df
    args = args[names(args)!="feature"]
    args$y = "Concentration"

    do.call(ggmetaplots::ggboxplot, args) +
        labs(title = feature)

}
################################################################################
#' @title Plot Histogram of Missing Values
#' @description This function takes a mSet object and print a histogram of
#' missing values. The x axis is the number of missing values per feature, and
#' the y axis is number of features with specific number of missing values.
#' This is usful when filling NAs.
#'
#' @param object A \code{\link{mSet-class}} or derived object.
#' @param include.zero Logic value whether to include zero in the histogram.
#' Default is FALSE.
#' @export
#' @return A ggplot object
#' @examples
#' # ADD_EXAMPLES_HERE
plot_hist_NA = function(object, include.zero = FALSE){
    if(!inherits(object, "mSet"))
        stop("Input object must inherit from mSet")

    df = data.frame(
        num_na = apply(object@conc_table, 1, function(x) sum(is.na(x)))) %>%
        rownames_to_column("feature_id")
    if(sum(df$num_na >0) == 0)
        stop("No NA detected. You are good to go!", call. = FALSE)

    if(!include.zero)
        df = filter(df, num_na > 0)

    df = sapply(seq(0, max(df$num_na), 1), function(x){
        c(x, sum(df$num_na == x))
    }) %>% t %>% as.data.frame %>%
        setNames(c("num_na", "count")) %>%
        mutate(text.position = count + max(count)/20,
               text = ifelse(count == 0, NA, count))

    ggplot(df) +
        geom_col(aes(x = num_na, y = count), width = 1) +
        geom_text(aes(x = num_na, y = text.position, label = text)) +
        scale_x_continuous(limits = c(0, max(df$num_na)+1),
                           breaks = seq(0, max(df$num_na)+1, 1)) +
        labs(x = "number of missing values per feature",
             y = "count of features",
             title = "Totel feature number: " %+% nfeatures(object)) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))
}
################################################################################
#' @title Plot quality control from a MetabolomcisSet object
#'
#' @description This function takes a MetabolomicsSet object and makes scatter plots using quality control samples
#'
#' @param object A \code{\link{MetabolomicsSet-class}} object.
#' @param mean A character string indicating the feature_data column of qc means.
#' @param sd A character string indicating the feature_data column of qc standard deviation.
#' @param cv A character string indicating the feature_data column of qc coefficient of variance.
#' @param log A logic variable whether to log-transfer mean or sd.
#' @export
#' @author Chenghao Zhu
plot_qc = function(object,
                   mean = "qc_mean",
                   sd = "sd_mean",
                   cv = "cv_mean",
                   log=TRUE){
    if(!inherits(object, "MetabolomicsSet"))
        stop("Only MetabolomicsSet or derieved classes supported",
             call. = TRUE)
    if(missing(mean)){
        stop("mean is required", call. = FALSE)
    }else if(!missing(sd) & missing(cv)){
        p = plot_qc_sd(object = object, mean = mean, sd = sd, log = log)
    }else if(missing(sd) & !missing(cv)){
        p = plot_qc_cv(object = object, mean = mean, cv = cv, log = log)
    }else if(!missing(sd) & !missing(cv)){
        p1 = plot_qc_sd(object = object, mean = mean, sd = sd, log = log)
        p2 = plot_qc_cv(object = object, mean = mean, cv = cv, log = log)
        p = cowplot::plot_grid(p1, p2, nrow = 1)
    }else{
        stop("At least either of sd or cv needs to be specified",
             call. = FALSE)
    }
    return(p)
}
#' @keywords internal
plot_qc_sd = function(object, mean, sd, log=TRUE){
    df = object@feature_data[,c(mean, sd)]
    if(log){
        df[, mean] = log(df[, mean] + 1)
        df[, sd]   = log(df[, sd]   + 1)
    }
    xlab = "Mean, Quality Controls"
    if(log) xlab = xlab %+% " (log)"
    ylab = "SD, Quality Controls"
    if(log) ylab = ylab %+% " (log)"
    ggplot(df) +
        geom_point(aes_string(x = mean, y = sd), alpha = 0.8) +
        labs(x = xlab, y = ylab) +
        theme_bw()
}
#' @keywords internal
plot_qc_cv = function(object, mean, cv, log=TRUE){
    df = object@feature_data[,c(mean, cv)]
    if(log) df[, mean] = log(df[, mean] + 1)
    xlab = "Mean, Quality Controls"
    if(log) xlab = xlab %+% " (log)"
    ggplot(df) +
        geom_point(aes_string(x = mean, y = cv), alpha = 0.8) +
        scale_y_continuous(limits = c(0,100)) +
        labs(x = xlab, y = "CV (%), Quality Controls")+
        theme_bw()
}

################################################################################
#' @title Plot normality boxplot of each individual feature
#'
#' @description This funciton first scale each feature using the \code{\link{scale}}
#' function, then make a boxplot of the z-score of each feature. This is usful
#' to check the overall normality of the dataset
#'
#' @param object A \code{\link{MetabolomicsSet-class}} object.
#' @param sort A boolean variable indicating whether arrange the features according
#' to their medians.
#'
#' @return A ggplot object
#' @export
#' @examples
#' data(lipid)
#' lipid = subset_features(lipid, 1:10)
#' plot_normality_bxoplot(lipid)
plot_normality_boxplot = function(object, sort = T){
    if(!inherits(object, "mSet")) {
        stop("[ Metabase ] Object must inherit form mSet")
    }
    conc_table = object$conc_table

    df = conc_table %>%
        apply(1, scale) %>% melt %>%
        group_by(Var2) %>% mutate(median = median(value)) %>%
        arrange(median) %>%
        ungroup()

    if(sort){
        df = mutate(df, Var2 = factor(Var2, levels = unique(Var2)))
    }

    p = ggplot(df, aes(x = Var2, y = value)) +
        geom_boxplot() +
        coord_flip() +
        labs(x = "", y = "z-score") +
        theme_bw()
    return(p)
}

################################################################################
#' @title Normality density plot of each feature
#'
#' @description This funciton first scale each feature using the \code{\link{scale}}
#' function, then make a density curve of the z-score of each feature. This is usful
#' to check the overall normality of the dataset.
#'
#' @param object A \code{\link{MetabolomicsSet-class}} object.
#' @param sort A boolean variable indicating whether arrange the features according
#' to their medians.
#'
#' @return A ggplot object
#' @export
#' @seealso \code{\link{geom_density_ridges_gradient}}
#' @examples
#' data(lipid)
#' lipid = subset_features(lipid, 1:10)
#' plot_normality_ridges(lipid)
plot_normality_ridges = function(object, sort = T){

    if(!requireNamespace("ggridges")){
        stop("[ Metabase ] Please install the ggridges package.")
    }

    if(!inherits(object, "mSet")) {
        stop("[ Metabase ] Object must inherit form mSet")
    }
    conc_table = object$conc_table

    df = conc_table %>%
        apply(1, scale) %>% melt %>%
        group_by(Var2) %>% mutate(median = median(value)) %>%
        arrange(median) %>%
        ungroup()

    if(sort){
        df = mutate(df, Var2 = factor(Var2, levels = unique(Var2)))
    }

    p = ggplot(df) +
        ggridges::geom_density_ridges_gradient(aes(x = value, y = Var2, fill = ..x..)) +
        scale_fill_viridis_c() +
        guides(fill = guide_colorbar(title = "z-score")) +
        labs(x = "z-score", y = "") +
        theme_bw()
    return(p)
}

################################################################################
#' @title Histogram of medians of all features after scaling.
#'
#' @description This function makes a histogram of the medians of all features
#' after scaling into z-scores. It is useful to check the overall normality
#' of a given dataset.
#'
#' @param object A \code{\link{MetabolomicsSet-class}} object.
#' @param bins Integer
#' @param fill Character, the color fo fill the columns.
#' @param density Boolean, whether to plot a density curve
#' @param density_color Character, the color of density curve.
#' @export
#' @examples
#' data(lipid)
#' plot_median_hist(lipid)

plot_median_hist = function(object, bins = 30, fill = "steelblue", density = TRUE,
                            density_color = "black"){

    if(!is.numeric(bins)){
        stop("bins must be numeric")
    }
    if(bins <= 0){
        stop("bins must be positive")
    }
    bins = round(bins)

    if(!inherits(object, "mSet")) {
        stop("[ Metabase ] Object must inherit form mSet")
    }
    conc_table = object$conc_table

    df = conc_table %>%
        apply(1, scale) %>% melt %>%
        group_by(Var2) %>%
        summarize(median = median(value)) %>%
        ungroup()

    p = ggplot(df, aes(x = median, y = ..density..)) +
        geom_histogram(bins = bins, color = "white", fill = fill)

    if(density){
        p = p + geom_density(color = density_color)
    }

    p = p + theme_bw()

    return(p)
}


################################################################################
#' @title Create a PCA plot
#' @description Create a PCA plot of an mSet object
#' @param object an mSet object
#' @param color character variable indicating which sample variable will be used
#' to give colors. If length is larger than 1, the two variables will be used
#' by calling \code{\link{interaction}}
#' @param ellipse boolean whether to draw ellipses.
#'
#' @seealso \code{\link[ggmetaplots]{ggscatterplot}}
#'
#' @examples
#'
#' plot_pca(lipid)
#' plot_pca(lipid, color = "Treatment")
#' plot_pca(lipid, color = c("Treatment", "Timepoint"), ellipse = TRUE)
#'
#' @export
plot_pca = function(object, color, ellipse = FALSE, ...){
    if(!inherits(object, "mSet")) {
        stop("[ Metabase ] object must inherits from mSet")
    }
    if(!requireNamespace("ggmetaplots")) {

    }

    pca = apply(object@conc_table, 1, scale) %>% prcomp
    sdev = pca$sdev ^ 2 / sum(pca$sdev ^ 2)
    xlab = "PC1 [ " %+% round(sdev[1] * 100, 2) %+% "% explained ]"
    ylab = "PC2 [ " %+% round(sdev[2] * 100, 2) %+% "% explained ]"

    data.frame(
        x = pca$x[, "PC1"],
        y = pca$x[, "PC2"]
    ) %>% cbind(object@sample_table) %>%
        ggmetaplots::ggscatterplot(
            "x", "y", color = color, ellipse = ellipse, trendline = FALSE, ...
        ) +
        labs(x = xlab, y = ylab)
}

################################################################################
#' @title Create a heatmap of an mSet object
#' @description Create a heatmap using an mSet object. This function requires the
#' zheatmap package.
#' @param object an mSet object
#' @param colSideBar string, the sample
#' @param scale string, must be one of sample, feature, or none.
#' @param scale.fun, string, must be one either scale or absolute_scale
#' @seealso \code{\link[zheatmap]{zheatmap}}
#'
#' @examples
#'
#' mset = subset_features(lipid, 1:25)
#' plot_heatmap(lipid)
#' plot_heatmap(lipid, colSideBar = "Treatment")
#' plot_heatmap(lipid, rowsSideBar = "class")
#'
#' @export
plot_heatmap = function(object,
                        colSideBar = NULL,
                        rowSideBar = NULL,
                        scale = "sample",
                        scale.fun = "scale",
                        ...){
    if(!inherits(object, "mSet")) {
        stop("[ Metabase ] object must inherits from mSet")
    }
    if(!requireNamespace("zheatmap")){
        stop("[ Metabase ] Please install the zheatmap:\n\n    devtools::install_github('zhuchcn/zheatmap')",
             call. = FALSE)
    }

    if(!is.null(colSideBar)) {
        if(length(colSideBar) == 1){
            colSideBar = object@sample_table[,colSideBar]
        } else {
            colSideBar = do.call(interaction, lapply(colSideBar, function(x) object@sample_table[[x]]))
        }
    }
    if(!is.null(rowSideBar)) {
        rowSideBar = object@feature_data[, rowSideBar]
    }

    if(!(scale %in% c("sample", "feature", "none"))){
        stop("scale must be one of \"sample\", \"feature\" and \"none\" ")
    }
    scale = switch(
        scale,
        "sample" = "row",
        "feature" = "column",
        "none" = "none"
    )

    if(!(scale.fun %in% c("scale", "absolute_scale"))){
        stop("scale.fun must be either \"scale\" or \"absolute_scale\" ")
    }

    zheatmap::zheatmap(object@conc_table, colSideBar = colSideBar,
                       rowSideBar = rowSideBar, scale = scale,
                       scale.fun = scale.fun, ...)
}
