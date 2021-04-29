#' Assessment of Normality and Linearity in Linear Regression
#'
#' This function is to examine normality and linearity of a linear regression in the form of histograms of your predictor and your outcome as well as a scatterplot of these variables.
#'
#' @param data The data frame that includes the variables you are interested in examining in a linear regression.
#' @param x The X variable you'd like to examine.
#' @param y Your outcome of interest.
#' @param alpha Value to determine how transparent you'd like your histograms and points in the scatterplot.
#' @param bins Value to adjust the bins of your histogram.
#' @param fill Value to determine the color you'd like your histogram to be filled with. The outline of the histograms is set to "White"
#' @param color Value to determine what color you'd like your points to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @param loess_color value to determine what color you'd like your loess line to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @param line_color value to determine what color you'd like your linear relationship to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @param se A logical vector to decide if you'd like to include the standard error of both the loess and linear relationships in your scatterplot.
#' @param size Value to decide if you'd like your lines to be thinner or thicker in your scatterplot
#' @return Returns three ggplot2 visuals. Two histograms of your variables of interest and a scatterplot of the relationship.
#' @export
#' @examples
#'
#' uni_bi_dist(data = mtcars, x = hp, y = mpg, alpha = .8, bins = 15, fill = "dodgerblue", color = "black", loess_color = "darkgreen", line_color = "green", se = FALSE, size = 1.25)

uni_bi_dist <- function(data, x, y, alpha, bins, fill, color, loess_color, line_color, se = c(TRUE, FALSE), size){

  x_histogram <- ggplot2::ggplot({{data}}, aes({{x}})) +
    ggplot2::geom_histogram(color = 'white', fill = {{fill}}, alpha = {{alpha}}, bins = {{bins}}) +
    ggplot2::labs(title = 'Histogram to Assess Normality of X') +
    ggplot2::theme_minimal()

  y_histogram <- ggplot2::ggplot({{data}}, aes({{y}})) +
    ggplot2::geom_histogram(color = 'white', fill = {{fill}}, alpha = {{alpha}}, bins = {{bins}}) +
    ggplot2::labs(title = 'Histogram to Assess Normality of Y') +
    ggplot2::theme_minimal()

  line_plot <- ggplot2::ggplot({{data}}, aes({{x}}, {{y}})) +
    ggplot2::geom_point(color = {{color}}, alpha = {{alpha}}) +
    ggplot2::geom_smooth(color = {{loess_color}}, se = {{se}}, size = {{size}}) +
    ggplot2::geom_smooth(method = 'lm', color = {{line_color}}, se = {{se}}, size = {{size}}) +
    ggplot2::labs(title = 'Scatterplot Between X and Y') +
    ggplot2::theme_minimal()

  gridExtra::grid.arrange(x_histogram, y_histogram, line_plot, nrow = 3)
}
