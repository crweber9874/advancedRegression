model = model_syntax_clpm(waves = 3, model_type = "ri-clpm")


data = simulate_riclpm(
  waves = 3,
  stability.p = 0.7,
  stability.q = 0.7,
  cross.p = 0.1,
  cross.q = 0.1,
  variance.p = 1,
  variance.q = 1,
  cov.pq = 0.25,
  beta.u = 0.01,
  sample.nobs = 5000)


model = lavaan(model, data$data)
coef(model)["kappa~~kappa"]

waves = 3
mean_px1 = 0.2
var_px1 = 0.1
sigma2_ux = 0.1
alpha_gg = 0.1
sigma2_gg = 0.1
sigma_ggpx1 = 0.1
beta_x = 0.1
phi_x = 0.1

waves = 10
# Specify the latent variable structure
# Factor
model_string <- "px1 =~ 1* x1"
for(w in 2:waves){
  model_string <- paste0(model_string, "\npx", w, "=~ 1*x", w)
}
# Means, first estimated, rest to 0
model_string <- paste0(model_string, " \n px1 ~ 0.33333 ")
for(w in 2:waves){
  model_string <- paste0(model_string, "\n px", w, " ~ 0 ")
}
# variances, first estimated, rest to 0
model_string <- paste0(model_string, " \n px1 ~~ 0.33333 * px1")
for(w in 2:waves){
  model_string <- paste0(model_string, "\n px", w, " ~~ 0 * ", "px", w)
}
# Specify x to have constant mean and variance
for(w in 1:waves){
  model_string <- paste0(model_string, "\n x", w, " ~ 0 ")
}
for(w in 1:waves){
  model_string <- paste0(model_string, " \n x", w, " ~~ 0.33333 * x", w)
}
# Autoregressions of latent variables
for(w in 2:waves){
  model_string <- paste0(model_string, "\n px", w, " ~ 1 * px", w-1)
}
# Change scores, from 2 to waves
for(w in 2:waves){
  model_string <- paste0(model_string, "\n dx", w, " =~ 1 * px", w)
}
for(w in 2:waves){
  model_string <- paste0(model_string, "\n dx", w, " ~ 0")
}
for(w in 2:waves){
  model_string <- paste0(model_string, "\n dx", w, " ~~ 0 * dx", w)
}

# No covariance between change scores
for(w in 3:waves){
  model_string <- paste0(model_string, "\n dx", w, " ~~ 0 * dx", w-1)
}
# Specify higher order, constant change factor
model_string <- paste0(model_string, " \ngg =~ ")
for(w in 2:waves){
  model_string <- paste0(model_string, "1 * dx", w, " ")
  if (w < waves) {
    model_string <- paste0(model_string, "+ ")
  }
}
model_string <- paste0(model_string, "\n gg ~ 0.33333 ")
model_string <- paste0(model_string, "\n gg ~~ 0.33333 * gg")
model_string <- paste0(model_string, "\n gg ~~ 0.33333 * px1")

# Add the proportional change

cat(model_string)

data = lavaan::simulateData(model_string)
model = lavaan(model_string, data)
#> # Specify autoregression of change score
#> dx3 ~ phi_x * dx2
#> dx4 ~ phi_x * dx3
#> dx5 ~ phi_x * dx4
#>
#>
#> # Specify latent true scores
#> lx1 =~ 1 * x1
#> lx2 =~ 1 * x2
#> lx3 =~ 1 * x3
#> lx4 =~ 1 * x4
#> lx5 =~ 1 * x5
#> # Specify mean of latent true scores
#> lx1 ~ gamma_lx1 * 1
#> lx2 ~ 0 * 1
#> lx3 ~ 0 * 1
#> lx4 ~ 0 * 1
#> lx5 ~ 0 * 1
#> # Specify variance of latent true scores
#> lx1 ~~ sigma2_lx1 * lx1
#> lx2 ~~ 0 * lx2
#> lx3 ~~ 0 * lx3
#> lx4 ~~ 0 * lx4
#> lx5 ~~ 0 * lx5
#> # Specify intercept of obseved scores
#> x1 ~ 0 * 1
#> x2 ~ 0 * 1
#> x3 ~ 0 * 1
#> x4 ~ 0 * 1
#> x5 ~ 0 * 1
#> # Specify variance of observed scores
#> x1 ~~ sigma2_ux * x1
#> x2 ~~ sigma2_ux * x2
#> x3 ~~ sigma2_ux * x3
#> x4 ~~ sigma2_ux * x4
#> x5 ~~ sigma2_ux * x5
#> # Specify autoregressions of latent variables
#> lx2 ~ 1 * lx1
#> lx3 ~ 1 * lx2
#> lx4 ~ 1 * lx3
#> lx5 ~ 1 * lx4
