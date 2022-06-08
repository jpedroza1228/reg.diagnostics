#' Palette of Car Colors
#'
#' Use \code{\link{car_color}} to construct palettes of desired length.
#'
#' @export

car_color_palettes <- list(
  datsun1 = c("#6a1f25", "#387448", "#d74122", "#7f5d47", "#669b3e", "#7fb7b7", "#ddca72"),
  datsun2 = c("#d72d2b", "#386350", "#507b8b", "#a86428", "#b23a2d"),
  datsun3 = c("#5b6242", "#2c6195", "#c0cc56", "#b23a2d")
)

#' This function includes color palettes of car companies that previous decades. More color palettes will be added in the future.
#'
#' @param name Name of the color palette. \code{datsun1} has 7 distinct colors from the Datsun motor company,
#' \code{datsun2} has 5 distinct colors and \code{datsun3} has 4 colors.
#' @param n The amount of colors desired for your plot.
#' @return A vector of colors.
#' @export
#' @examples
#'
#' car_color(name = "datsun1", n = 4)
#' car_color(name = "datsun2")
#'
#' library(tidyverse)
#' mtcars %>%
#' ggplot(aes(hp, mpg)) +
#' geom_point(aes(color = as.factor(cyl))) +
#' scale_color_manual(values = car_color("datsun2"))

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

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb

print_pal <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

car_color_palettes

