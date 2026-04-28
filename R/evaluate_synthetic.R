#' Compute pMSE, SPECKS and PO50 in one call
#'
#' Convenience wrapper that computes the three propensity score-based utility
#' metrics comparing original data (`OD`) and synthetic data (`SD`).
#'
#' By default, the function returns only the metric value and p-value for each
#' measure. Analysts can optionally request the critical threshold and/or the
#' null-hypothesis decision.
#'
#' @param OD A data frame containing the original data.
#' @param SD A data frame containing the synthetic data.
#' @param alpha Significance level used in all three metrics.
#' @param split Proportion of the combined sample used for training in `SPECKS`
#'   and `PO50`.
#' @param seed Optional integer seed for reproducible sample splitting in
#'   `SPECKS` and `PO50`.
#' @param include_threshold Logical; if `TRUE`, includes the threshold(s) for
#'   each metric in the output.
#' @param include_decision Logical; if `TRUE`, includes `H0_accepted` for each
#'   metric in the output.
#'
#' @return A named list with one element per metric: `pMSE`, `SPECKS`, and
#'   `PO50`.
#'
#' @examples
#' set.seed(123)
#' OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#' SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#'
#' evaluate_synthetic(OD, SD)
#' evaluate_synthetic(OD, SD, include_threshold = TRUE, include_decision = TRUE)
#'
#' @export
evaluate_synthetic <- function(
  OD,
  SD,
  alpha = 0.05,
  split = 0.5,
  seed = NULL,
  include_threshold = FALSE,
  include_decision = FALSE
) {
  pmse_res <- pMSE(OD = OD, SD = SD, alpha = alpha)
  specks_res <- SPECKS(OD = OD, SD = SD, split = split, alpha = alpha, seed = seed)
  po50_res <- PO50(OD = OD, SD = SD, split = split, alpha = alpha, seed = seed)

  list(
    pMSE = .select_output_fields(pmse_res, include_threshold, include_decision),
    SPECKS = .select_output_fields(specks_res, include_threshold, include_decision),
    PO50 = .select_output_fields(po50_res, include_threshold, include_decision)
  )
}
