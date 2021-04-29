#' Examining assumptions of a Logistic Regression
#'
#' This function is to examine linearity and outliers of a logistic regression.
#' @param data The data frame that includes the variables you are interested in examining in a logistic regression.
#' @param x Predictors to be included in your linear regression. You can either include one variable in quotations (e.g., "hp" from the mtcars dataset)
#' or you can create an object of predictors (e.g., predictors <- c("disp", "cyl", "wt")).
#' @param y Your outcome of interest.
#' @param alpha Value to determine how transparent you'd like your points in this function's plots.
#' @param se A logical vector to decide if you'd like to include the standard error for your plots.
#' @param size Value to determine the size of the points in this function's plots. You can also determine if you'd like to assign these values to a
#' categorical variable in your dataset.
#' @param line_size Value to decide if you'd like your lines to be thinner or thicker in your plots
#' @param color Value to determine what color you'd like your points to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @param loess_color value to determine what color you'd like your loess line to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @param line_color value to determine what color you'd like your linear relationship to be in the scatterplot (e.g., "blue", "#6a1f25")
#' @return Returns two ggplot2 visuals. One for the assumption of linearity predicting logit of the outcome and the second to check for outliers in the residuals.
#' @export
#' @examples
#'
#' glm_assump(data = mtcars, x = "hp", y = vs, alpha = .5, se = FALSE, size = 2, line_size = 1.25, color = "dodgerblue", loess_color = "red", line_color = "blue")
#'
#' predictors <- c("hp", "carb", "gear")
#' glm_assump(data = mtcars, x = predictors, y = vs, alpha = .5, se = FALSE, size = 2, line_size = 1.25, color = "dodgerblue", loess_color = "red", line_color = "blue")

glm_assump <- function(data, x, y, alpha, se = c(TRUE, FALSE), size, line_size, color, loess_color, line_color){

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
    tidyr::gather(key = "predict_variables", value = "predictor_values", -logit, -{{y}})

  assumption <- data %>%
    ggplot2::ggplot(aes(logit, predictor_values)) +
    ggplot2::geom_point(size = {{size}}, alpha = {{alpha}}, color = {{color}}) +
    ggplot2::geom_smooth(method = "loess", se = {{se}}, size = {{line_size}}, color = {{loess_color}}) +
    ggplot2::geom_smooth(method = "lm", se = {{se}}, size = {{line_size}}, color = {{line_color}}) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(~predict_variables, scales = "free_y")

  model_data <- broom::augment(model) %>%
    dplyr::mutate(index = 1:n())

  outlier <- model_data %>%
    ggplot2::ggplot(aes(index, .std.resid)) +
    ggplot2::geom_point(aes(color = {{y}}), alpha = {{alpha}}) +
    ggplot2::theme_minimal()

  gridExtra::grid.arrange(assumption, outlier, nrow = 2)
}
