#' Examining some assumptions of a Logistic Regression
#'
#' This function is to examine linearity and outliers of a logistic regression.
#' @param data The data frame that includes the variables you are interested in examining in a logistic regression.
#' @param x Predictors to be included in your logistic regression. You can either include one variable in quotations (e.g., "hp" from the mtcars dataset)
#' or you can create an object of predictors (e.g., predictors <- c("disp", "cyl", "wt")).
#' @param y Your outcome of interest.
#' @param alpha Value to determine how transparent you'd like your points in this function's plots.
#' @param se A logical vector to decide if you'd like to include the standard error for your plot(s).
#' @param loess_color value to determine what color you'd like your loess line to be in the scatterplot.
#' @param line_color value to determine what color you'd like your linear relationship to be in the scatterplot.
#' @return Returns three ggplot2 visuals. One for the assumption of linearity predicting logit values of the outcome, the second to check for outliers in the residuals, and the third that looks at cook's distance.
#' @export
#' @examples
#'
#' glm_assump(data = mtcars, x = "hp", y = vs)
#'
#' predictors <- c("hp", "mpg")
#' glm_assump(data = mtcars, x = predictors, y = vs)

glm_assump <- function(data,
                       x,
                       y,
                       alpha = .5,
                       se = FALSE,
                       loess_color = 'dodgerblue',
                       line_color = 'black'){

  library(magrittr)

  y <- deparse(substitute(y))

  ivs <- paste(x, collapse = " + ")
  my_formula <- as.formula(paste(y, "~", ivs))
  model <- glm(my_formula, data = data, family = "binomial")

  plot_data <- predict(model, type = "response")
  model_predictors <- colnames(data)

  data <- {{data}} %>%
    tidyr::drop_na({{x}}, {{y}}) %>%
    dplyr::select({{x}}, {{y}}) %>%
    dplyr::mutate(logit = log(plot_data/(1 - plot_data))) %>%
    tidyr::pivot_longer(cols = !logit,
                        names_to = "predict_variables",
                        values_to = "predictor_values") #change to pivot longer

  scatterplot <- data %>%
    ggplot2::ggplot(aes(logit, predictor_values)) +
    ggplot2::geom_point(alpha = {{alpha}}) +
    ggplot2::geom_smooth(method = "loess", se = {{se}}, color = {{loess_color}}) +
    ggplot2::geom_smooth(method = "lm", se = {{se}}, color = {{line_color}}) +
    ggplot2::facet_wrap(vars(predict_variables),
                        scales = "free_y")

  model_data <- broom::augment(model) %>%
    dplyr::mutate(index = 1:n())

  outlier <- model_data %>%
    ggplot2::ggplot(aes(index, .resid)) +
    ggplot2::geom_point(aes(color = {{y}}), alpha = {{alpha}}) +
    ggplot2::labs(title = 'Residuals') +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = 'none')

  cooks <- model_data %>%
    ggplot2::ggplot(aes(index, .cooksd)) +
    ggplot2::geom_point(aes(color = {{y}}), alpha = {{alpha}}) +
    ggplot2::labs(title = "Cook's Distance") +
    ggplot2::coord_flip() +
    ggplot2::theme(legend.position = 'none')


  message('Make sure your outcome is binary.')

  gridExtra::grid.arrange(scatterplot, outlier, cooks, nrow = 3)
}
