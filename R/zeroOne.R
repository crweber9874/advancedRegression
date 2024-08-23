#' Create a zero one normalization
#'
#' @param numeric value
#'
#' @return A standardized 0-1 variable
#' @export
#'
#' @examples #rnorm(10) %>% zero.one()
zero.one <- function(x) {

  min.x <- min(x, na.rm = T)
  max.x <- max(x - min.x, na.rm = T)
  return((x - min.x) / max.x)
}
