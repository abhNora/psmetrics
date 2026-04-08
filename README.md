# psmetrics

`psmetrics` is an R package to compute three propensity score-based utility metrics for comparing original data (OD) and synthetic data (SD):

- `pMSE`
- `SPECKS`
- `PO50`

It also provides `evaluate_synthetic()` to compute all three metrics in a single call.

## Authors  

- [Nora Amama-BenHassun](https://orcid.org/0009-0004-7598-0100)
- [Daniel Fernández](https://orcid.org/0000-0003-0012-2094)
- [Jordi Cortés](https://orcid.org/0000-0002-3764-0795)
- [Cecilio Angulo](https://orcid.org/0000-0001-9589-8199)

## Installation

To install the package locally from the package directory:

```r
remotes::install_local(".")
```

To install it from GitHub:

```r
remotes::install_github("abhNora/psmetrics")
```

## Example

```r
library(psmetrics)

set.seed(123)
OD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))
SD <- data.frame(x = rnorm(100), y = rbinom(100, 1, 0.5))

evaluate_synthetic(OD, SD)
evaluate_synthetic(OD, SD, include_threshold = TRUE, include_decision = TRUE)
```

## Development workflow

Install the development dependencies:

```r
install.packages(c("roxygen2", "remotes", "rcmdcheck"))
```


## Generate documentation

```r
roxygen2::roxygenise()
```

## Install the package locally and validate it:

```r
remotes::install_local(".")
rcmdcheck::rcmdcheck(args = c("--no-manual"), build_args = c("--no-manual"))
```

## Testing

To run the package tests locally:

```r
# Install testing dependencies if needed
install.packages(c("testthat", "rcmdcheck"))

# Run tests from the package root
testthat::test_local()

# Full package validation
rcmdcheck::rcmdcheck(args = c("--no-manual"), build_args = c("--no-manual"))

## Funding

