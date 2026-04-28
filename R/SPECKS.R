#' Compute the SPECKS utility metric
#'
#' Computes the SPECKS metric comparing an original dataset (`OD`) and a
#' synthetic dataset (`SD`). To reduce overfitting, the propensity model is fit
#' on a training subset and evaluated on a test subset.
#'
#' @param OD A data frame containing the original data.
#' @param SD A data frame containing the synthetic data.
#' @param split Proportion of the combined sample used for training.
#' @param alpha Significance level used to compute the critical threshold.
#' @param seed Optional integer seed for reproducible sample splitting.
#'
#' @return A named list with:
#' \describe{
#'   \item{metric}{Character string with the metric name.}
#'   \item{value}{Observed SPECKS value.}
#'   \item{threshold}{Critical value for the Kolmogorov-Smirnov comparison.}
#'   \item{H0_accepted}{Logical value indicating whether the null hypothesis is accepted.}
#'   \item{p_value}{Associated p-value from the KS test.}
#' }
#'
#' @details
#' SPECKS is computed as the maximum absolute difference between the empirical
#' cumulative distribution functions of propensity scores for original and
#' synthetic records in the test split.
#'
#' @examples
#' set.seed(123)
#' OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#' SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
#'
#' SPECKS(OD, SD, seed = 123)
#'
#' @export
SPECKS <- function(OD, SD, split = 0.5, alpha = 0.05, seed = NULL) {
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

  ecdf_od <- stats::ecdf(df_test$propensity[df_test$label == 0])
  ecdf_sd <- stats::ecdf(df_test$propensity[df_test$label == 1])

  all_scores <- sort(unique(df_test$propensity))
  specks_value <- max(abs(ecdf_od(all_scores) - ecdf_sd(all_scores)))

  n_eff <- n1_test * n2_test / (n1_test + n2_test)
  k_alpha <- sqrt(-0.5 * log(alpha / 2))
  critical_threshold <- k_alpha / sqrt(n_eff)

  p_value <- stats::ks.test(
    df_test$propensity[df_test$label == 0],
    df_test$propensity[df_test$label == 1],
    exact = FALSE
  )$p.value

  h0_accepted <- specks_value <= critical_threshold

  .format_metric_output(
    metric_name = "SPECKS",
    value = specks_value,
    threshold = critical_threshold,
    h0_accepted = h0_accepted,
    p_value = p_value
  )
}
