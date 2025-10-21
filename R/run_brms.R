#' Launch BRMS Regression Shiny App
#'
#' @param sample_data Optional. If TRUE, loads mtcars into global environment
#' @param ... Additional arguments passed to shiny::runApp
#'
#' @export
#' @examples
#' if (interactive()) {
#'   # Load some data first
#'   data(mtcars)
#'   data(iris)
#'
#'   # Then run the app
#'   run_brms_app()
#' }
run_brms_app <- function(sample_data = FALSE, ...) {

  # Optionally load sample data
  if (sample_data) {
    # Load sample datasets into global environment
    data(mtcars, envir = .GlobalEnv)
    data(iris, envir = .GlobalEnv)
    message("Loaded sample datasets: mtcars, iris")
  }

  # Check if any data frames exist in workspace
  data_check <- ls(envir = .GlobalEnv)
  has_data <- any(sapply(data_check, function(x) {
    is.data.frame(get(x, envir = .GlobalEnv))
  }))

  if (!has_data) {
    warning("No data frames found in workspace. Load data before running the app, or use run_brms_app(sample_data = TRUE)")
  }

  # Find and run the app
  appDir <- system.file("shinyapp", package = "yourpackage")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the package.",
         call. = FALSE)
  }

  shiny::runApp(appDir, ...)
}
