#' Assessment of Univariate and Bivariate Distribution Between A Predictor and Outcome
#'
#' This function examines the univariate and bivariation distributions through the use of histograms for the predictor and outcome, as well as a scatterplot between the predictor and the outcome.
#'
#' @param data The data frame that includes the variables you are interested in examining in a linear regression.
#' @param x The predictor you'd like to examine.
#' @param y The outcome of interest.
#' @param bins Value to adjust the bins of your histogram.
#' @param alpha Value to determine how transparent you'd like the raw values in the scatterplot.
#' @param se A logical vector to decide if you'd like to include the standard error for the linear, quadratic, and cubic relationships
#' @param quad_color Value of the color to determine if there is a quadratic relationship, #d74122 from the car_color_palettes is the default
#' @param cubic_color Value of the color to determine if there is a cubic relationship, #669b3e from the car_color_palettes is the default
#' @param size Value to decide the size of each line.
#' @return Returns three ggplot2 visuals. Two histograms of your variables of interest and a scatterplot of the relationship.
#' @export
#' @examples
#'
#' uni_bi_dist(data = mtcars, x = hp, y = mpg)

uni_bi_dist <- function(data,
                        x,
                        y,
                        bins = 15,
                        alpha = .5,
                        se = FALSE,
                        quad_color = '#d74122',
                        cubic_color = '#669b3e',
                        size = 1.25){

  x_histogram <- ggplot2::ggplot({{data}},
                                 aes({{x}})) +
    ggplot2::geom_histogram(color = 'white',
                            bins = {{bins}}) +
    ggplot2::labs(title = 'Histogram of Predictor')

  y_histogram <- ggplot2::ggplot({{data}},
                                 aes({{y}})) +
    ggplot2::geom_histogram(color = 'white',
                            bins = {{bins}}) +
    ggplot2::labs(title = 'Histogram of Outcome')

  line_plot <- ggplot2::ggplot({{data}},
                               aes({{x}},
                                   {{y}})) +
    ggplot2::geom_point(alpha = {{alpha}}) +
    ggplot2::geom_smooth(method = 'lm',
                         se = {{se}},
                         color = 'black',
                         size = {{size}}) +
    ggplot2::geom_smooth(method = 'lm',
                         se = {{se}},
                         formula = y ~ poly(x, 2),
                         color = {{quad_color}},
                         size = {{size}}) +
    ggplot2::geom_smooth(method = 'lm',
                         se = {{se}},
                         formula = y ~ poly(x, 3),
                         color = {{cubic_color}},
                         linetype = 2,
                         size = {{size}}) +
    ggplot2::labs(title = 'Scatterplot Between X and Y')

  gridExtra::grid.arrange(gridExtra::arrangeGrob(x_histogram,
                                                 y_histogram,
                                                 ncol = 1,
                                                 nrow = 2),
                          gridExtra::arrangeGrob(line_plot,
                                                 ncol = 1,
                                                 nrow = 1),
                          heights=c(4,1), widths=c(1,1))
}
