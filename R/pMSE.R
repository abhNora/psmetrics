#' Compute the pMSE utility metric
#'
#' Computes the propensity-score mean squared error (pMSE) comparing an
#' original dataset (`OD`) and a synthetic dataset (`SD`). This implementation
#' uses the full combined sample to fit the propensity model, as in the source
#' code provided for the paper.
#'
#' @param OD A data frame containing the original data.
#' @param SD A data frame containing the synthetic data.
#' @param alpha Significance level used to compute the critical threshold.
#'
#' @return A named list with:
#' \describe{
#'   \item{metric}{Character string with the metric name.}
#'   \item{value}{Observed pMSE value.}
#'   \item{threshold}{Critical value for the test under the null hypothesis.}
#'   \item{H0_accepted}{Logical value indicating whether the null hypothesis is accepted.}
#'   \item{p_value}{Associated p-value.}
#' }
#'
#' @details
#' The function stacks `OD` and `SD`, creates a binary label (`0` for `OD`,
#' `1` for `SD`), fits a logistic regression, and computes pMSE as:
#'
#' `pMSE = (1 / N) * sum((propensity - c)^2)`
#'
#' where `c` is the proportion of synthetic records in the combined sample.
#'
#' @examples
#' set.seed(123)
#' OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#' SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#'
#' pMSE(OD, SD)
#'
#' @export
pMSE <- function(OD, SD, alpha = 0.05) {
  .validate_metric_inputs(OD, SD)

  q <- ncol(OD)
  OD$label <- 0
  SD$label <- 1
  df <- rbind(OD, SD)

  logit_model <- stats::glm(as.factor(label) ~ ., data = df, family = stats::binomial())
  df$propensity <- stats::predict(logit_model, newdata = df, type = "response")

  n1 <- nrow(df[df$label == 0, , drop = FALSE])
  n2 <- nrow(df[df$label == 1, , drop = FALSE])
  c_prop <- n2 / (n1 + n2)
  N <- n1 + n2

  pmse_value <- (1 / N) * sum((df$propensity - c_prop)^2)

  scale_fac <- ((1 - c_prop) * c_prop) / N
  upper_threshold <- stats::qchisq(1 - alpha, df = q) * scale_fac

  statistic <- pmse_value / scale_fac
  p_value <- stats::pchisq(statistic, df = q, lower.tail = FALSE)
  h0_accepted <- pmse_value <= upper_threshold

  .format_metric_output(
    metric_name = "pMSE",
    value = pmse_value,
    threshold = upper_threshold,
    h0_accepted = h0_accepted,
    p_value = p_value
  )
}
