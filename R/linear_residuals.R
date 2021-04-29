#' Assessment of Residuals in Linear Regression
#'
#' This function is to examine residuals to assess for homoscedasticity and normality in a linear regression.
#' This function allows for the examination of residuals of a bivariate linear regression or a multiple linear regression.
#'
#' @param data The data frame that includes the variables you are interested in examining in a linear regression.
#' @param x Predictors to be included in your linear regression. You can either include one variable in quotations (e.g., "hp" from the mtcars dataset)
#' or you can create an object of predictors (e.g., predictors <- c("disp", "cyl", "wt")).
#' @param y Your outcome of interest in your linear regression. This variable does not require quotations.
#' @param se A logical vector to decide if you'd like to include the standard error of your linear regression.
#' @param alpha Value to determine how transparent you'd like your points in this function's plots.
#' @param size Value to determine the size of the points in this function's plots. You can also determine if you'd like to assign these values to a
#' categorical variable in your dataset.
#' @param line_size Value to decide if you'd like your lines to be thinner or thicker in this function's plots.
#' @param color Value to determine what color you'd like your points to be in this function's plots (e.g., "blue", "#6a1f25")
#' @param line_color value to determine what color you'd like your lines to be in this function's plots (e.g., "blue", "#6a1f25")
#' @return Returns two ggplot2 visuals showing the residuals in your model.
#' @export
#' @examples
#'
#' One Predictor
#' residual_view(data = mtcars, x = "hp", y = mpg, se = FALSE, alpha = .3, size = 2, line_size = 1, color = "blue", line_color = "dodgerblue")
#'
#' predictors <- c("hp", "carb", "gear")
#' residual_view(data = mtcars, x = predictors, y = mpg, se = FALSE, alpha = .3, size = 2, line_size = 1, color = "blue", line_color = "dodgerblue")

linear_residuals <- function(data, x, y, se = c(TRUE, FALSE), alpha, size, line_size, color, line_color){

  y <- deparse(substitute(y))

  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- lm(my_formula, data = data)

  fort <- ggplot2::fortify(model)

  res_plot <- ggplot2::ggplot(fort, aes(.fitted, .resid)) +
    ggplot2::geom_point(color = {{color}}, alpha = {{alpha}}, size = {{size}}) +
    ggplot2::geom_smooth(method = "lm", color = {{line_color}}, se = {{se}}, size = {{line_size}}) +
    ggplot2::labs(title = "Plot of Residual Errors") +
    ggplot2::theme_minimal()

  qq_plot_model <- ggplot2::ggplot(model, aes(sample = .stdresid)) +
    ggplot2::geom_qq(color = {{color}}, size = {{size}}, alpha = {{alpha}}) +
    ggplot2::stat_qq_line(size = {{line_size}}, color = {{line_color}}) +
    ggplot2::theme_minimal()

  message("Make sure there is not a pattern in your Residuals")

  gridExtra::grid.arrange(res_plot, qq_plot_model, nrow = 2)

}
