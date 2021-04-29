#' Function to get proportions and percentages of a single variable
#'
#' This function is to examine normality and linearity of a linear regression in the form of histograms of your predictor and your outcome as well as a scatterplot of these variables.
#'
#' @param data The data frame that includes the factor variable you are interested in getting proportions, percentages, and a visual of groups in the variable.
#' @param x The X variable you'd like to examine.
#' @param fill Value to determine the color you'd like your bar graph to be filled with. The outline of the histograms is set to "White"
#' @param total Numerical value of your total sample in your study.
#' @return Returns a list with a visual of the groups and a table of the proportions and percentages
#' @export
#' @examples
#'
#' To examine the visual
#' freq_bar(mtcars, as.factor(cyl), fill = "dodgerblue", total = 32)[[1]]
#'
#' To create a table
#' freq_bar(mtcars, as.factor(cyl), fill = "dodgerblue", total = 32)[[2]]

freq_bar <- function(data, x, fill, total){

  library(magrittr)

  bar_plot <- ggplot2::ggplot({{data}}, aes({{x}})) +
    ggplot2::geom_bar(color = "white", fill = {{fill}}) +
    ggplot2::theme_minimal()

  summary <- {{data}} %>%
    dplyr::group_by({{x}}) %>%
    dplyr::summarize(n = n(),
              prop = n/total,
              percent = prop*100)

  table <- reactable::reactable(summary)

  return(list(bar_plot, table))

}
