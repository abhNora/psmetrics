#' Validate input data for PS metrics
#'
#' Internal helper that checks whether original and synthetic datasets are valid
#' inputs for the package.
#'
#' @param OD A data frame containing the original data.
#' @param SD A data frame containing the synthetic data.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @noRd
.validate_metric_inputs <- function(OD, SD) {
  if (!is.data.frame(OD) || !is.data.frame(SD)) {
    stop("`OD` and `SD` must both be data frames.", call. = FALSE)
  }

  if (ncol(OD) != ncol(SD)) {
    stop("`OD` and `SD` must have the same number of columns.", call. = FALSE)
  }

  if (!identical(names(OD), names(SD))) {
    stop("`OD` and `SD` must have identical column names and ordering.", call. = FALSE)
  }

  if (nrow(OD) < 2 || nrow(SD) < 2) {
    stop("`OD` and `SD` must each contain at least two rows.", call. = FALSE)
  }

  TRUE
}

#' Format the output of a metric function
#'
#' Internal helper to standardize metric outputs.
#'
#' @param metric_name Name of the metric.
#' @param value Numeric metric value.
#' @param threshold Numeric threshold or named numeric vector of thresholds.
#' @param h0_accepted Logical decision for the null hypothesis.
#' @param p_value Numeric p-value.
#'
#' @return A named list.
#' @noRd
.format_metric_output <- function(metric_name, value, threshold, h0_accepted, p_value) {
  out <- list(
    metric = metric_name,
    value = unname(value),
    threshold = threshold,
    H0_accepted = isTRUE(h0_accepted),
    p_value = unname(p_value)
  )

  class(out) <- c("ps_metric_result", class(out))
  out
}

#' Subset metric output fields
#'
#' Internal helper used by `evaluate_synthetic()`.
#'
#' @param result A metric result list.
#' @param include_threshold Logical; whether to include thresholds.
#' @param include_decision Logical; whether to include the hypothesis decision.
#'
#' @return A reduced named list.
#' @noRd
.select_output_fields <- function(result, include_threshold = FALSE, include_decision = FALSE) {
  out <- list(
    value = result$value,
    p_value = result$p_value
  )

  if (isTRUE(include_threshold)) {
    out$threshold <- result$threshold
  }

  if (isTRUE(include_decision)) {
    out$H0_accepted <- result$H0_accepted
  }

  out
}
