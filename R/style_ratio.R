#' Style significant figure-like rounding for ratios
#'
#' When reporting ratios, such as relative risk or an odds ratio, we'll often
#' want the rounding to be similar on each side of the number 1.  For example,
#' if we report an odds ratio of 0.95 with a confidence interval of 0.70 to 1.24,
#' we would want to round to two decimal places for all values. In other words,
#' 2 significant figures for numbers less than 1 and 3 significant figures 1 and
#' larger. `style_ratio()` performs significant figure-like rounding in this manner.
#'
#' @param x Numeric vector
#' @param digits Integer specifying the number of significant
#' digits to display for numbers below 1. Numbers larger than 1 will be be `digits + 1`.
#' Default is `digits = 2`.
#' @inheritParams style_number
#' @export
#' @return A character vector of styled ratios
#' @family style tools
#' @author Daniel D. Sjoberg
#' @examples
#' c(
#'   0.123, 0.9, 1.1234, 12.345, 101.234, -0.123,
#'   -0.9, -1.1234, -12.345, -101.234
#' ) %>%
#'   style_ratio()
style_ratio <- function(x, digits = 2, big.mark = NULL, decimal.mark = NULL, ...) {
  x_fmt <-
    case_when(
      round2(abs(x), digits = digits) < 1 ~
        style_sigfig(x, digits = digits, big.mark = big.mark, decimal.mark = decimal.mark, ...),
      x > 0 ~
        style_sigfig(pmax(1, x), digits = digits + 1, big.mark = big.mark, decimal.mark = decimal.mark, ...),
      x < 0 ~
        style_sigfig(pmin(-1, x), digits = digits + 1, big.mark = big.mark, decimal.mark = decimal.mark, ...),
    )

  attributes(x_fmt) <- attributes(unclass(x))
  x_fmt
}
