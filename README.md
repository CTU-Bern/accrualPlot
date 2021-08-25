
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `accrualPlot` <img src='man/figures/sticker.png' align="right" height="200">

<!-- [![](https://www.r-pkg.org/badges/version/accrualPlot?color=green)](https://cran.r-project.org/package=accrualPlot)  -->

[![](https://img.shields.io/badge/dev%20version-0.6.2-blue.svg)](https://github.com/CTU-Bern/accrualPlot)
[![Actions
Status](https://github.com/CTU-Bern/accrualPlot/workflows/R-CMD-check/badge.svg)](https://github.com/CTU-Bern/accrualPlot/actions)
<!-- ![travis](https://travis-ci.com/CTU-Bern/presize.svg?branch=master) -->
<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/CTU-Bern/presize?branch=master&svg=true)](https://ci.appveyor.com/project/CTU-Bern/presize) -->
<!-- [![codecov](https://codecov.io/github/CTU-Bern/accrualPlot/branch/master/graphs/badge.svg)](https://codecov.io/github/CTU-Bern/accrualPlot) -->

Accrual plots are an important tool when monitoring clinical trials.
Some trials are terminated early due to low accrual, which is a waste of
resources (including time). Assessing accrual rates can also be useful
for planning analyses and estimating how long a trial needs to continue
recruiting participants. `accrualPlot` provides tools for such plots

## Installation

<!-- `accrualPlot` can be installed from CRAN in the usual manner: -->

The package can be installed from the CTU Bern universe via

``` r
install.packages('accrualPlot', repos = 'https://ctu-bern.r-universe.dev')
```

## Overview

The first step to using `accrualPlot` is to create an accrual dataframe.
This is simply a dataframe with a counts of participants included per
day.

``` r
# load package
library(accrualPlot)
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union

# generate some data
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)

df <- accrual_create_df(x)
```

Cumulative and absolute recruitment plots , as well as a method to
predict the time point of study completion, are included.

``` r
par(mfrow = c(1,3))
plot(df, which = "cum")
plot(df, which = "abs")
plot(df, which = "pred", target = 100)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
