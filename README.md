
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jbutils

<!-- badges: start -->
<!-- badges: end -->

The goal of jbutils is to be a repository of utility functions.

## Installation

You can install the development version of jbutils like so:

``` r
# install.packages("devtools")
devtools::install_github("jeremy-p-b/jbutils")
```

## Usage

### Wilson CI for a binomial proportion

You can use jbutils to calculate a Wilson CI for a binomial proportion.

``` r
library(jbutils)
wilson_lower(15,80)
#> [1] 0.1170531
```
