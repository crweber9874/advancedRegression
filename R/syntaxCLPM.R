#' Generate model syntax for a cross-lagged panel model, with or without random intercepts
#'
#'
#' @param waves The number of waves (time points) in the model.
#' @param model_type Specify whether an RI-CLPM or CLPM.
#'
#' @return character string containing the Lavaan model syntax.
#'
#' @export
#'

model_syntax_clpm = function(
    waves = 10,
    model_type = c('clpm', 'ri-clpm')) {
  model_string <- "kappa =~ 1* x1"
  for(w in 2:waves){
    model_string <- paste0(model_string, " + 1 * x", w)
  }

  model_string <- paste0(model_string, "\n omega =~ 1* y1")
  for(w in 2:waves){
    model_string <- paste0(model_string, " + 1 * y", w, "")
  }



  if(model_type == "clpm"){
  # latent variable covariances and variances
        model_string <- paste0(model_string,
                         "\nkappa ~~", "0 * kappa
                                         \nomega ~~", "0* omega
                                         \nkappa ~~", "0* omega")
  }
  if(model_type == "ri-clpm"){
    model_string <- paste0(model_string,
                           "\nkappa ~~", "kappa
                                         \nomega ~~", "omega
                                         \nkappa ~~", "omega")
  }

  # Loadings, 1 for identification with 1 observed, latent variable by wave

  for(w in 1:waves){
    model_string <- paste0(model_string, "\np", w, " =~ 1*x", w,
                           "\nq", w, " =~ 1*y", w)
  }

  for(w in waves:2){
    model_string <- paste0(model_string, "\n p", w, " ~ ", "  p", w-1, " + ",  "  q", w-1,
                           "\n q", w, " ~ ", "  q", w-1, " + ", "  p", w-1)

  }
  model_string <- paste0(model_string, "\np1 ~~   p1 \n q1 ~~   q1 \n p1 ~~   q1")

  # Set variances of latent variable residuals
  for(w in 2:waves){
    model_string <- paste0(model_string, "\n p", w, " ~~ ", "pv* p", w,
                           "\n q", w, " ~~ ",  "  qv * q", w,
                           "\n p", w, " ~~ ",  "  q", w)
  }


  return(model_string)
}
