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
plot_hist_missing_values = function(object, include.zero = FALSE){
    df = data.frame(
        num_na = apply(object@conc_table, 1, function(x) sum(is.na(x)))) %>%
        rownames_to_column("feature_id")
    if(!include.zero)
        df = filter(df, num_na > 0)
    df = group_by(df, num_na) %>%
        summarize(count = n()) %>%
        ungroup() %>%
        mutate(text.position = count + max(count)/20)
    ggplot(df) +
        geom_col(aes(x = num_na, y = count)) +
        geom_text(aes(x = num_na, y = text.position, label = count)) +
        theme_bw()
}
