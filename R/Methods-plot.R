################################################################################
#' @title Make a boxplot of a feature
#' @description
#' This function uses the ggplot2 package to generate a boxplot from a givin
#' \code{\link{mSet-class}} object by specifying a feature id. The feature id
#' must come from the feature names
#' @param object An \code{\link{mSet-class}} object
#' @param feature A character indicating the feature name to plot. Must be one
#' of the feature names.
#' @param by A character vector with length up to 3, indicating The sample
#' sample meta-data variable to plot the concentration againt to. It must be
#' from the sample data's column names. If multiple variables are givin, facets
#' will be used.
#' @param line A character of a sample meta-data variable. If specified,
#' geom_line will be called to draw lines between 2 points. This is particually
#' usful to deal with repeated measures.
#' @param color.by A character of a sample meta-data variable. If specified,
#' points with different levels will be colored diffently. See examples.
#' @param jitter numeric. If specified, points will be jittered. Recommanded
#' value: 0.15. the \code{line} and \code{jitter} can not be specified at the
#' same time.
#' @param point.size numeric. The size of points. Default is 3
#' @param point.alpha numeric. The transparency of points.
#' @param point.color character. If the \code{color.by} is not specified, this
#' value will be given the to the points color.
#' @param whisker.width numeirc. The width of boxplot whisker. Default is 0.5.
#' @param color.pal character. The color panel to use.
#' @param show.legend logical. Whether to show legend. Default is TRUE.
#' @param syle character. The pre-defined style to apply on the plot. "bw" is a
#' empty default style using the \code{\link{theme_bw}}. "academic" is a classic
#' style based on the \code{\link{theme_classic}}.
#' @param plotly logical. If TRUE, a plotly variable will be returned.
#' @author Chenghao Zhu
#' @import ggplot2
#' @export
plot_boxplot = function(object,
                        feature,
                        by            = NULL,
                        show.points   = TRUE,
                        line          = NULL,
                        color.by      = NULL,
                        jitter        = 0,
                        box.size      = 0.5,
                        whisker.size  = 0.5,
                        whisker.width = 0.5,
                        point.size    = 3,
                        point.alpha   = 0.5,
                        point.color   = "black",
                        color.pal     = NULL,
                        show.legend   = TRUE,
                        style         = "bw",
                        plotly        = FALSE){

    sample_vars = unique(c(by, color.by, line))

    df = data.frame(
        Concentration = object@conc_table[feature,]
    ) %>%
        cbind(object@sample_table[,sample_vars])

    # points
    my_geom_point = function(){
        if(is.null(color.by)){
            geom_point(size = point.size, alpha = point.alpha,
                       color = point.color,
                       position = position_jitter(w=jitter))
        }else{
            geom_point(aes_string(color = color.by),
                       size = point.size, alpha = point.alpha,
                       position = position_jitter(w=jitter))
        }
    }
    # define the theme function
    my_theme = function(){
        if(style == "bw"){
            theme_bw() +
                theme(
                    strip.text = element_text(size = 13)
                )
        }else if(style == "academic"){
            theme_classic() +
                theme (
                    panel.border = element_rect(size = 1 , fill = NA),
                    strip.text   = element_text(size = 13),
                    axis.text.x  = element_text(size = 12, color = "black"),
                    axis.text.y  = element_text(size = 11, color = "black"),
                    axis.ticks   = element_line(size = 1 , color = "black"),
                    axis.title.x = element_text(size = 15, vjust = -2),
                    axis.title.y = element_text(size = 15, vjust = 2),
                    plot.margin  = margin(l = 15,  b = 15,
                                          t = 10,  r = 10, unit  = "pt")
                )
        }
    }

    # to do: add point.color
    p = ggplot(df,aes_string(x = by[1], y = "Concentration")) +
        geom_boxplot(outlier.shape = NA, size = box.size) +
        stat_boxplot(geom = "errorbar", width = whisker.width,
                     size = whisker.size) +
        my_theme()
    # show points
    if(show.points){
        p = p + my_geom_point()
    }

    # line
    if(!is.null(line))
        p = p + geom_line(aes_string(group = line, color = color.by))
    # facet
    if(length(by) == 2 ){
        p = p + facet_grid(rows = by[2])
    }else if(length(by) == 3){
        p = p + facet_grid(rows = by[2], cols = by[3])
    }
    # color panel
    if(!is.null(color.pal)){
        col_num = length(unique(df[,color]))
        mypal = colorRampPalette(colors = color.pal)
        p = p + scale_color_manual(
            values = mypal(col_num))
    }
    # hide legend
    if(!show.legend)
        p = p + theme(legend.position = "none")
    # plotly
    if(plotly) p = plotly::ggplotly(p)

    return(p)
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
#'
#' @examples
#' # ADD_EXAMPLES_HERE
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






