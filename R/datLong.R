#' Reshape Data from Wide to Long Format for Cross-Lagged Analysis
#'
#' This function takes a wide-format data frame containing variables named 'x1', 'x2', ..., 'xn', and 'y1', 'y2', ..., 'yn', and reshapes it into a long format suitable for cross-lagged analysis. The resulting data frame includes columns for individual ID, wave, x, y, xlag (lagged x), and ylag (lagged y).
#'
#' @param data A wide-format data frame containing variables named 'x1', 'x2', ..., 'xn', and 'y1', 'y2', ..., 'yn'.
#' @return A long-format data frame with columns 'id', 'wave', 'x', 'y', 'xlag', and 'ylag'.
#'
#' @examples
#'
#'
#' dat = simulate_observed_clr(waves = 3)$data
#'
#' head(dat)
#'
#' @importFrom dplyr %>% select mutate row_number group_by ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#'
#' @export


reshape_long_sim_cr <- function(data = dat) {
  y <- data %>%
    dplyr::select(tidyr::contains("y")) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = starts_with("y"))
  names(y) <- c("id", "y_var", "y")

  x <- data %>%
    dplyr::select(tidyr::contains("x")) %>%
    tidyr::pivot_longer(cols = starts_with("x"))
  names(x) <- c("x_var", "x")

  # Join x and y
  dat <- cbind(x, y) %>%
    dplyr::mutate(wave = readr::parse_number(x_var)) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      xlag = dplyr::lag(x, order_by = wave),
      ylag = dplyr::lag(y, order_by = wave)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, wave, x, y, xlag, ylag)

  return(dat)
}


