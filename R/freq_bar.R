#' Function to get proportions and percentages of a single variable
#'
#' This function calculates the proportion and perception of the variable chosen. Additionally, the function also provides a bar graph for the variable chosen.
#'
#' @param data The data frame that includes the variable you are interested in getting proportions, percentages, and a visual of groups in the variable.
#' @param x The variable you'd like to examine.
#' @param fill The color you'd like to make your bar graph columns. The default is dodgerblue and the outline is set to white.
#' @return Returns a list with a bar graph and a table of the proportions and percentages.
#' @export
#' @examples
#'
#' To examine the visual.
#' freq_bar(mtcars, cyl))[[1]]
#'
#' To create a table.
#' freq_bar(mtcars, cyl, fill = 'darkgreen')[[2]]
#'
#' To examine both. In RStudio, visual will be in Plots, while the table will be in Viewer.
#' freq_bar(mtcars, cyl)
#'
#' Can also use tidyverse syntax with pluck to retrieve visual or table.
#' mtcars %>%
#' freq_bar(cyl) %>%
#' pluck(1)

freq_bar <- function(data,
                     x,
                     fill = 'dodgerblue'){

  library(magrittr)

  bar_plot <- ggplot2::ggplot({{data}},
                              aes({{x}})) +
    ggplot2::geom_bar(color = "white",
                      fill = {{fill}})

  prop_sum <- {{data}} %>%
    dplyr::group_by({{x}}) %>%
    dplyr::summarize(n = n(),
                     prop = n/nrow({{data}}),
                     percent = prop*100)

  table <- reactable::reactable(prop_sum) %>%
    reactablefmtr::add_title('Proportions & Percentages')

  return(list(Plot = bar_plot,
              Table = table))

}

