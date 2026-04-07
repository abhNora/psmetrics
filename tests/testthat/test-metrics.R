# Tests for psmetrics package

library(testthat)
library(psmetrics)

setup_test_data <- function() {
  set.seed(123)
  
  OD <- data.frame(
    x1 = rnorm(100, mean = 5, sd = 2),
    x2 = rnorm(100, mean = 10, sd = 3),
    y  = rbinom(100, 1, 0.5)
  )
  
  SD_similar <- data.frame(
    x1 = rnorm(100, mean = 5.1, sd = 2.1),
    x2 = rnorm(100, mean = 10.1, sd = 3.1),
    y  = rbinom(100, 1, 0.52)
  )
  
  SD_different <- data.frame(
    x1 = rnorm(100, mean = 15, sd = 5),
    x2 = rnorm(100, mean = 20, sd = 8),
    y  = rbinom(100, 1, 0.8)
  )
  
  list(
    OD = OD,
    SD_similar = SD_similar,
    SD_different = SD_different
  )
}

test_that("pMSE returns the expected output structure", {
  data <- setup_test_data()
  result <- pMSE(data$OD, data$SD_similar)
  
  expect_type(result, "list")
  expect_true(all(c("metric", "value", "threshold", "H0_accepted", "p_value") %in% names(result)))
  
  expect_identical(result$metric, "pMSE")
  expect_true(is.numeric(result$value))
  expect_length(result$value, 1)
  expect_true(is.finite(result$value))
  
  expect_true(is.numeric(result$threshold))
  expect_length(result$threshold, 1)
  expect_true(is.finite(result$threshold))
  
  expect_type(result$H0_accepted, "logical")
  expect_length(result$H0_accepted, 1)
  
  expect_true(is.numeric(result$p_value))
  expect_length(result$p_value, 1)
  expect_true(is.finite(result$p_value))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("SPECKS returns the expected output structure", {
  data <- setup_test_data()
  result <- SPECKS(data$OD, data$SD_similar, seed = 123)
  
  expect_type(result, "list")
  expect_true(all(c("metric", "value", "threshold", "H0_accepted", "p_value") %in% names(result)))
  
  expect_identical(result$metric, "SPECKS")
  expect_true(is.numeric(result$value))
  expect_length(result$value, 1)
  expect_true(is.finite(result$value))
  expect_true(result$value >= 0 && result$value <= 1)
  
  expect_true(is.numeric(result$threshold))
  expect_length(result$threshold, 1)
  expect_true(is.finite(result$threshold))
  
  expect_type(result$H0_accepted, "logical")
  expect_length(result$H0_accepted, 1)
  
  expect_true(is.numeric(result$p_value))
  expect_length(result$p_value, 1)
  expect_true(is.finite(result$p_value))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("PO50 returns the expected output structure", {
  data <- setup_test_data()
  result <- PO50(data$OD, data$SD_similar, seed = 123)
  
  expect_type(result, "list")
  expect_true(all(c("metric", "value", "threshold", "H0_accepted", "p_value") %in% names(result)))
  
  expect_identical(result$metric, "PO50")
  expect_true(is.numeric(result$value))
  expect_length(result$value, 1)
  expect_true(is.finite(result$value))
  
  expect_true(is.numeric(result$threshold))
  expect_true(all(c("lower", "upper") %in% names(result$threshold)))
  expect_length(result$threshold, 2)
  expect_true(all(is.finite(result$threshold)))
  
  expect_type(result$H0_accepted, "logical")
  expect_length(result$H0_accepted, 1)
  
  expect_true(is.numeric(result$p_value))
  expect_length(result$p_value, 1)
  expect_true(is.finite(result$p_value))
  expect_true(result$p_value >= 0 && result$p_value <= 1)
})

test_that("pMSE is larger for more different synthetic data", {
  data <- setup_test_data()
  
  pmse_similar <- pMSE(data$OD, data$SD_similar)
  pmse_different <- pMSE(data$OD, data$SD_different)
  
  expect_true(pmse_different$value > pmse_similar$value)
})

test_that("evaluate_synthetic returns only value and p_value by default", {
  data <- setup_test_data()
  result <- evaluate_synthetic(data$OD, data$SD_similar, seed = 123)
  
  expect_type(result, "list")
  expect_true(all(c("pMSE", "SPECKS", "PO50") %in% names(result)))
  
  expect_identical(names(result$pMSE), c("value", "p_value"))
  expect_identical(names(result$SPECKS), c("value", "p_value"))
  expect_identical(names(result$PO50), c("value", "p_value"))
})

test_that("evaluate_synthetic includes threshold when requested", {
  data <- setup_test_data()
  result <- evaluate_synthetic(
    data$OD,
    data$SD_similar,
    seed = 123,
    include_threshold = TRUE
  )
  
  expect_identical(names(result$pMSE), c("value", "p_value", "threshold"))
  expect_identical(names(result$SPECKS), c("value", "p_value", "threshold"))
  expect_identical(names(result$PO50), c("value", "p_value", "threshold"))
})

test_that("evaluate_synthetic includes threshold and decision when requested", {
  data <- setup_test_data()
  result <- evaluate_synthetic(
    data$OD,
    data$SD_similar,
    seed = 123,
    include_threshold = TRUE,
    include_decision = TRUE
  )
  
  expect_identical(names(result$pMSE), c("value", "p_value", "threshold", "H0_accepted"))
  expect_identical(names(result$SPECKS), c("value", "p_value", "threshold", "H0_accepted"))
  expect_identical(names(result$PO50), c("value", "p_value", "threshold", "H0_accepted"))
  
  expect_type(result$pMSE$H0_accepted, "logical")
  expect_type(result$SPECKS$H0_accepted, "logical")
  expect_type(result$PO50$H0_accepted, "logical")
})

test_that("split-sample metrics are reproducible with a fixed seed", {
  data <- setup_test_data()
  
  result1_specks <- SPECKS(data$OD, data$SD_similar, seed = 456)
  result2_specks <- SPECKS(data$OD, data$SD_similar, seed = 456)
  
  result1_po50 <- PO50(data$OD, data$SD_similar, seed = 456)
  result2_po50 <- PO50(data$OD, data$SD_similar, seed = 456)
  
  expect_equal(result1_specks$value, result2_specks$value)
  expect_equal(result1_specks$threshold, result2_specks$threshold)
  expect_equal(result1_specks$H0_accepted, result2_specks$H0_accepted)
  expect_equal(result1_specks$p_value, result2_specks$p_value)
  
  expect_equal(result1_po50$value, result2_po50$value)
  expect_equal(result1_po50$threshold, result2_po50$threshold)
  expect_equal(result1_po50$H0_accepted, result2_po50$H0_accepted)
  expect_equal(result1_po50$p_value, result2_po50$p_value)
})

test_that("functions error on mismatched columns", {
  data <- setup_test_data()
  SD_wrong <- data.frame(z = rnorm(100))
  
  expect_error(pMSE(data$OD, SD_wrong), "same number of columns|identical column names")
  expect_error(SPECKS(data$OD, SD_wrong), "same number of columns|identical column names")
  expect_error(PO50(data$OD, SD_wrong), "same number of columns|identical column names")
})

test_that("functions error on empty data frames", {
  OD_empty <- data.frame()
  SD_empty <- data.frame()
  
  expect_error(pMSE(OD_empty, SD_empty))
  expect_error(SPECKS(OD_empty, SD_empty))
  expect_error(PO50(OD_empty, SD_empty))
})

test_that("functions error when split is invalid", {
  data <- setup_test_data()
  
  expect_error(SPECKS(data$OD, data$SD_similar, split = 0), "between 0 and 1")
  expect_error(SPECKS(data$OD, data$SD_similar, split = 1), "between 0 and 1")
  
  expect_error(PO50(data$OD, data$SD_similar, split = 0), "between 0 and 1")
  expect_error(PO50(data$OD, data$SD_similar, split = 1), "between 0 and 1")
})
