#' Compute the PO50 utility metric
#'
#' Computes the PO50 metric comparing an original dataset (`OD`) and a
#' synthetic dataset (`SD`). To reduce overfitting, the propensity model is fit
#' on a training subset and evaluated on a test subset.
#'
#' @param OD A data frame containing the original data.
#' @param SD A data frame containing the synthetic data.
#' @param split Proportion of the combined sample used for training.
#' @param alpha Significance level used to compute the critical thresholds.
#' @param seed Optional integer seed for reproducible sample splitting.
#'
#' @return A named list with:
#' \describe{
#'   \item{metric}{Character string with the metric name.}
#'   \item{value}{Observed PO50 value.}
#'   \item{threshold}{Named numeric vector with lower and upper critical bounds.}
#'   \item{H0_accepted}{Logical value indicating whether the null hypothesis is accepted.}
#'   \item{p_value}{Associated two-sided p-value.}
#' }
#'
#' @details
#' After training the propensity model on the training split, records in the
#' test split are classified using the estimated propensity scores. The PO50
#' value is then computed from the percentage of correctly classified records,
#' rescaled by subtracting 50.
#'
#' @examples
#' set.seed(123)
#' OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#' SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#'
#' PO50(OD, SD, seed = 123)
#'
#' @export
PO50 <- function(OD, SD, split = 0.5, alpha = 0.05, seed = NULL) {
  .validate_metric_inputs(OD, SD)

  if (!is.numeric(split) || length(split) != 1 || split <= 0 || split >= 1) {
    stop("`split` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  OD$label <- 0
  SD$label <- 1
  df <- rbind(OD, SD)

  idx <- caret::createDataPartition(y = seq_len(nrow(df)), p = split, list = FALSE)
  df_train <- df[idx, , drop = FALSE]
  df_test <- df[-idx, , drop = FALSE]

  logit_model <- stats::glm(as.factor(label) ~ ., data = df_train, family = stats::binomial())
  df_test$propensity <- stats::predict(logit_model, newdata = df_test, type = "response")

  n1_test <- nrow(df_test[df_test$label == 0, , drop = FALSE])
  n2_test <- nrow(df_test[df_test$label == 1, , drop = FALSE])
  c_test <- n2_test / (n1_test + n2_test)
  N_test <- n1_test + n2_test

  df_test$predicted_label <- ifelse(df_test$propensity >= c_test, 1, 0)
  correct_classifications <- sum(df_test$predicted_label == df_test$label)

  po50_value <- ((correct_classifications / nrow(df_test)) * 100) - 50

  p0 <- c_test^2 + (1 - c_test)^2
  lower_k <- stats::qbinom(alpha / 2, N_test, p0)
  upper_k <- stats::qbinom(1 - alpha / 2, N_test, p0)
  lower_threshold <- (100 * lower_k / N_test) - 50
  upper_threshold <- (100 * upper_k / N_test) - 50

  p_lower <- stats::pbinom(correct_classifications, size = N_test, prob = p0)
  p_upper <- stats::pbinom(correct_classifications - 1, size = N_test, prob = p0, lower.tail = FALSE)
  p_value <- min(2 * min(p_lower, p_upper), 1)
  h0_accepted <- !(po50_value < lower_threshold || po50_value > upper_threshold)

  .format_metric_output(
    metric_name = "PO50",
    value = po50_value,
    threshold = c(lower = lower_threshold, upper = upper_threshold),
    h0_accepted = h0_accepted,
    p_value = p_value
  )
}
