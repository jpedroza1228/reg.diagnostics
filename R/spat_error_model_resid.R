#' Examining errors in a spatial error model.
#'
#' This function is to investigate into the residuals of a spatial error model visually and with the Moran's I statistic.
#' @param data The data.frame that includes a geometry column, uses the st_sf function from the sf package.
#' @param model The spatial error model that was run to examine the residuals.
#' @param color The color of the line for the residual visualization.
#' @param listw The spatial weights based on neighbors from the spdep package.
#' @param zero_policy Additional argument where Null uses the global option, FALSE stops due to an error for any empty neighbor sets, and TRUE will create a weights list with zero-length weights vectors. Requires
#' @param nsim The number of random permutations for the Moran's test.
#' @param seed A seed to make sure the permutations are consistent when running this function.
#' @return Returns a visualization of the residuals and the global Moran's I statistic.
#' @export
#' @examples
#'
#' spat_error_model_resid(data = data, model = model, listw = listw, zero.policy = TRUE, seed = 12345)

spat_error_model_resid <- function(data,
                                   model,
                                   color = 'dodgerblue',
                                   listw,
                                   zero_policy = c(NULL, TRUE, FALSE),
                                   nsim = 9999,
                                   seed = 12345){

  visual <- ggplot2::ggplot(data = data, aes(x = model$residuals, y = model$fitted.values)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = mean(model$fitted.values),
                        color = color)

  set.seed(seed)
  moran_find <- spdep::moran.mc(model$residuals, listw = listw, zero.policy = zero_policy, nsim = nsim)

  statistics <- if(moran_find$statistic < 0){
    upper <- (1 - moran_find$p.value)
    upper*2

  } else{
    moran_find$p.value*2
  }

  message("p value for Moran's I statistic is for a two-tailed test.")
  warning('Model should be a spatial error model.')
  warning("A significant Moran's I indicates autocorrelation in the residuals.")

  return(list(visual, statistics))
}
