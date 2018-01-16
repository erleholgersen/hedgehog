#' stripboxplot
#'
#' @description 
#'  Make a boxplot with underlying stripplot
#' 
#' @param plot.formula Formula for plotting
#' @param dataset Dataset
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param ... additional parameters to be passed to \code{stripchart}
#' 
#' @export stripboxplot
stripboxplot <- function(
    plot.formula, 
    dataset = NULL,
    xlab = NULL,
    ylab = NULL,
    ...
    ) {
    
    stripchart(
        plot.formula,
        dataset, 
        vertical = TRUE,
        method = 'jitter',
        jitter = 0.35,
        col = 'lightgrey',
        pch = 19,
        xlab = xlab,
        ylab = ylab,
        ...
        );
    
    boxplot(
        plot.formula,
        dataset,
        add = TRUE,
        col = 'transparent',
        outline = FALSE
        );
    
}