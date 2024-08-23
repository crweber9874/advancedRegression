#' Simulate data from an observed cross-lagged regression (CLR) model
#'
#' Create a simulated dataset of a cross-lagged model with a specified number of waves and structural parameters.
#'
#' @param waves The number of waves (time points) in the model.
#' @param beta.u The effect of the unobserved variable (u) on the observed variables (x and y).
#' @param beta.z The effect of the instrument (z) on the x variable.
#' @param contemporaneous.x The effect of the x variable on the y variable at the same time point.
#' @param var.x The variance of the x variable.
#' @param var.y The variance of the y variable.
#' @param cov.uz The covariance between the unobserved variable (u) and the instrument (z).
#' @param stability.x The stability parameter for the x variable (autoregressive effect).
#' @param stability.y The stability parameter for the y variable (autoregressive effect).
#' @param cov.xy The covariance between x and y within the same time point.
#' @param cross.x The cross-lagged effect of x on y at the next time point.
#' @param cross.y The cross-lagged effect of y on x at the next time point.
#' @param ... Additional arguments to pass to the `lavaan::simulateData` function.
#'
#' @return A list containing two elements:
#'    * `model`: The Lavaan model syntax used for data simulation.
#'    * `data`: The simulated data in a data frame format.
#'

#' @export

simulate_observed_clr <- function(waves = 10,
                                stability.x = 0.9,
                                stability.y = 0.9,
                                contemporaneous.x = 0.5,
                                cross.x = 0.3,
                                beta.z = 0.5,
                                beta.u = 0.5,
                                var.y = 1,
                                var.x = 1,
                                cov.xy = 0.5,
                                cov.uz = 0,
                                ...) {
  model_string <- paste0("
    u ~~ ", cov.uz, "*z

    # Regression equations for the first wave
    x1 ~ ", beta.u, "*u + ", beta.z, "*z
    y1 ~ ", beta.u, "*u + ", beta.z, "*z + ", contemporaneous.x, "*x1
                         ")

  # Add equations for subsequent waves
  if (waves > 1) {
    for (w in 2:waves) {
      model_string <- paste0(model_string, "
            x", w, " ~ ", beta.u,"*u +", beta.z,"*z + ", stability.x, "*x", w - 1, "
            y", w, " ~ ", beta.u,"*u +", beta.z,"*z + ", contemporaneous.x, "*x", w,"+", cross.x, "*x", w - 1, "+", stability.y,"*y", w-1)
    }
  }

  # Variances of observed variables (all errors set to 1)
  for (w in 1:waves) {
    model_string <- paste0(model_string, "
        x", w, " ~~ ", var.x, "*x", w, "
        y", w, " ~~ ", var.y, "*y", w, "
        ")
  }

  #Covariances
  for (w in 1:waves) {
    model_string <- paste0(model_string, "
        x", w, " ~~ ", cov.xy,"* y", w, "
        ")
  }
  dat = lavaan::simulateData(model_string, ...)

  return(list(model = model_string, data = dat))
}

