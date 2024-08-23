#' Simulate data from an observed, random intercept cross-lagged regression (RI-CLR) model
#'
#' Create a simulated dataset of a cross-lagged model with a specified number of waves and structural parameters.
#'
#' @param waves The number of waves (time points) in the model.
#' @param stability.p The stability parameter for the x variable (autoregressive effect).
#' @param stability.q The stability parameter for the y variable (autoregressive effect).
#' @param cov.pq The covariance between x and y within the same time point.
#' @param cross.q The cross-lagged effect of x on y at the next time point.
#' @param cross.p The cross-lagged effect of y on x at the next time point.
#' @param ... Additional arguments to pass to the `lavaan::simulateData` function.
#'
#' @return A list containing two elements:
#'    * `model`: The Lavaan model syntax used for data simulation.
#'    * `data`:  The simulated data in a data frame format.
#'

#' @export
#'
simulate_riclpm = function(
         waves = 10,
         stability.p = 0.9,
         stability.q = 0.9,
         cross.p = 0.1,
         cross.q = 0.1,
         variance.p = 1,
         variance.q = 1,
         cov.pq = 0.5,
         beta.u = 0.5,
         ...) {
                model_string <- "kappa =~ 1* x1"
                for(w in 2:waves){
                  model_string <- paste0(model_string, " + 1 * x", w)
                }

                model_string <- paste0(model_string, "\n omega =~ 1* y1")
                for(w in 2:waves){
                  model_string <- paste0(model_string, " + 1 * y", w, "")
                }

                #Intercepts
                for(w in 1:waves){
                  model_string <- paste0(model_string, "\nx", w, " ~ mu", w, "*1",
                                                       "\ny", w, " ~ pi", w, "*1")

                }

                # latent variable covariances and variances
                model_string <- paste0(model_string,
                                        "\nkappa ~~", 0.25, "* kappa
                                         \nomega ~~", 0.25, "* omega
                                         \nkappa ~~", 0.25, "* omega")


                # Loadings, 1 for identification with 1 observed, latent variable by wave

                for(w in 1:waves){
                  model_string <- paste0(model_string, "\np", w, " =~ 1*x", w,
                                                       "\nq", w, " =~ 1*y", w)
                  }

                for(w in waves:2){
                  model_string <- paste0(model_string, "\n p", w, " ~ ", stability.p, " * p", w-1, " + ",  cross.q, " * q", w-1,
                                                       "\n q", w, " ~ ", stability.q, " * q", w-1, " + ",  cross.p, " * p", w-1)

                }

                for(w in 1:waves){
                  model_string <- paste0(model_string, "\n p", w, " ~~ ", variance.p," * p", w,
                                                       "\n q", w, " ~~ ", variance.q, " * q", w,
                                                       "\n p", w, " ~~ ", cov.pq, " * q", w)


                }

                for(w in 1:waves){
                  model_string <- paste0(model_string, "\n p", w, " ~ ", 1,"  * U",
                                                       "\n q", w, " ~ ", 1, " * U")

                }

                model_string <- paste0(model_string, "\n U ~~ ", beta.u, "*U")
                dat = lavaan::simulateData(model_string, ...)

                return(list(model = model_string, data = dat))


return(model_string)
}


