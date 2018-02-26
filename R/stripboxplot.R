#' stripboxplot
#'
#' @description 
#'  Make a boxplot with underlying stripplot
#' 
#' @param plot.formula Formula for plotting
#' @param dataset Dataset
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param points.col Colour of background points
#' @param ... Additional parameters to be passed to \code{stripchart}
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' 
#' @examples
#' test.data <- data.frame(
#'  x = sample(letters[1:3], 50, replace = TRUE),
#'  y = rnorm(50)
#'  );
#' 
#' stripboxplot(y ~ x, test.data);
#' 
#' @export stripboxplot
stripboxplot <- function(
    plot.formula, 
    dataset = NULL,
    xlab = NULL,
    ylab = NULL,
    points.col = 'lightgrey',
    ...
    ) {
    
    stripchart(
        plot.formula,
        dataset, 
        vertical = TRUE,
        method = 'jitter',
        jitter = 0.35,
        col = points.col,
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
        outline = FALSE,
        xaxt = 'n',
        yaxt = 'n'
        );
    
}