# psrmetrics

`psmetrics` is an R package to compute three propensity score-based utility
metrics for comparing original data (OD) and synthetic data (SD):

- `pMSE`
- `SPECKS`
- `PO50`

It also provides `evaluate_synthetic()` to compute all three metrics in a
single call.

## Installation

```r
# install.packages("devtools")
devtools::install_local("path/to/psmetrics")
```

## Example

```r
library(psmetrics)

set.seed(123)
OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))

compute_psr_metrics(OD, SD)
compute_psr_metrics(OD, SD, include_threshold = TRUE, include_decision = TRUE)
```

## Generate documentation

```r
roxygen2::roxygenise("path/to/psrmetrics")
```
