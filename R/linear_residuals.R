#' Assessment of Residuals in Linear Regression
#'
#' This function is to examine residuals to assess for homoscedasticity and normality in a linear regression.
#' This function allows for the examination of residuals of a bivariate linear regression or a multiple linear regression.
#'
#' @param data The data frame that includes the variables you are interested in examining in a linear regression.
#' @param x Predictor(s) to be included in your linear regression. You can either include one variable in quotations (e.g., "hp" from the mtcars dataset)
#' or you can create an object of predictors in quotations (e.g., predictors <- c("disp", "cyl", "wt")).
#' @param y Your outcome of interest in your linear regression. This variable does not require quotations.
#' @param alpha Value to determine how transparent you'd like your points in this function's plots.
#' @param line_size Value to decide the size for the horizontal line in your residual plot.
#' @return Returns two ggplot2 visuals showing the residuals and the qqplot for your model.
#' @export
#' @examples
#'
#' One Predictor
#' residual_view(data = mtcars,
#'                x = "hp",
#'                y = mpg)
#'
#'cyl_dum <- as.data.frame(psych::dummy.code(mtcars$cyl))
#'
#'new_cars <- cbind(mtcars, cyl_dum)
#'
#'new_cars <- new_cars %>%
#'rename(cyl4 = `4`,
#'       cyl6 = `6`,
#'       cyl8 = `8`)
#'
#'predictors <- c('hp', 'wt', 'cyl6', 'cyl8')
#'
#' residual_view(data = mtcars,
#'                x = predictors,
#'                y = mpg)

linear_residuals <- function(data,
                             x,
                             y,
                             alpha = .5,
                             line_size = 1.25){

  y <- deparse(substitute(y))

  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- lm(my_formula, data = data)

  fort <- ggplot2::fortify(model)

  res_plot <- ggplot2::ggplot(fort, aes(.fitted, .resid)) +
    ggplot2::geom_point(alpha = {{alpha}}) +
    ggplot2::geom_hline(yintercept = 0, size = {{line_size}}) +
    ggplot2::labs(title = "Plot of Residual Errors")

  qq_plot_model <- ggplot2::ggplot(model, aes(sample = .stdresid)) +
    ggplot2::geom_qq(alpha = {{alpha}}) +
    ggplot2::stat_qq_line(size = {{line_size}})

  gridExtra::grid.arrange(res_plot, qq_plot_model, nrow = 2)

}
