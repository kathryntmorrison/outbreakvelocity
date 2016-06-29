
#' Take the derivatives of the polynomial model
#'
#' Gets the functions of the partial derivatives with respect to X and Y to estimate the front-wave velocity of the outbreak
#' @param m An lm model object
#' @param measure The measure of model fit, defaults to R-squared, 'r2', can be 'aic' or 'bic'
#' @param x Units indicating the position in x-axis of the outbreak points
#' @param y Units indicating the position in y-axis of the outbreak points
#' @param y Units indicating the position in y-axis of the outbreak points
#' @param order The order of the polynomial selected for the surface trend
#' @export
outbreak_rate_functions = function(m, measure='r2', x, y, order) {

  coefs.x = c("X")
  coefs.y = c("Y")
  if (order > 1) {
    coefs.x = c("X", paste0("X",2:order))
    coefs.y = c("Y", paste0("Y",2:order))
  }

  d.dx = m$coef['XY']*(y)
  d.dy = m$coef['XY']*(x)

  for(i in 1:length(coefs.x)) {

    d.dx = d.dx + (i) * m$coef[coefs.x][i] * x^(i - 1)
    d.dy = d.dy + (i) * m$coef[coefs.y][i] * y^(i - 1)
  }

  return(list(d.dx=d.dx, d.dy=d.dy))
}
