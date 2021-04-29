#' Palette of Car Colors
#'
#' This function includes color palettes of car companies that previous decades. More color palettes will be added in the future.
#'
#' @param name Name of the color palette. \code{datsun1} has 7 distinct colors from the Datsun motor company. \code{datsun2} has 5 distinct colors and \code{datsun3} has four colors.
#' @param n The amount of colors desired for your plot.
#' @return A vector of colors.
#' @export
#' @examples
#'
#' car_color("datsun1", 4)
#' car_color("datsun2")
#'
#' library(tidyverse)
#' mtcars %>%
#' ggplot(aes(hp, mpg)) +
#' geom_point(aes(color = as.factor(cyl))) +
#' scale_color_manual(values = car_color("datsun1"))

car_color_palettes <- list(datsun1 = c("#6a1f25", "#387448", "#7fb7b7", "#669b3e", "#ddca72", "#7f5d47", "#d74122"),
                           datsun2 = c("#d72d2b", "#386350", "#507b8b", "#a86428", "#b23a2d"),
                           datsun3 = c("#5b6242", "#2c6195", "#c0cc56", "#b23a2d"))

car_color <- function(name, n) {

  pal <- car_color_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- pal[1:n]

  structure(out, class = "palette", name = name)
}
