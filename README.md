# rcens Package

[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fdlsaavedra%2Frcens&count_bg=%2379C83D&title_bg=%23555555&icon=rstudio.svg&icon_color=%23FFFFFF&title=Views&edge_flat=true)](https://hits.seeyoufarm.com)

This package provides functions to generate censored samples of type I, II and III, from any random sample generator. It also provides the option to create left and right censorship Along with this, the generation of samples with interval censoring is in the testing phase. With two options of fixed length intervals and random lengths.

## Installation

You can install [rcens](https://CRAN.R-project.org/package=rcens) from CRAN via:

``` r
install.packages("rcens")
```

Or install the latest development version (on GitHub) via `{devtools}`:

``` r
devtools::install_github("dlsaavedra/rcens")
```

## Getting Started

Create easily a new censored data set establishment the percentage of censoring and the original distribution of random variable.

### Right Censored Data, type III (Random)

In this example create a right censored data type III and fit Kaplan Meier (survival library).

``` r
#Example Exponential - Uniform

devtools::install_github("dlsaavedra/rcens")
library(rcens)
library(survival)

Data = rcensT3(rdistrX = rexp, pdistrC = punif, rdistrC = runif,
                 param_X = list("rate" = 2),
                 param_C = list("min" = 0, "max" = "lambda"),
                 n = 1e02, theta = .5, right = TRUE)

S = Surv(Data$sample_censored,Data$censored_indicator, type = "right")
s1 = survfit(S ~ 1)

CDF_censored = ecdf(Data$sample_censored)
Survival_CDF = Vectorize(function(x){ 1 - CDF_censored(x)})
CDF_original= ecdf(Data$sample_uncensored)
Survival_CDF_original = Vectorize(function(x){ 1 - CDF_original(x)})

plot(Survival_CDF, col = "blue", xlim = c(0,2))
title("Survival Curve")
plot(Survival_CDF_original, col = "red", add= TRUE, xlim = c(0,2))
lines(s1$time, s1$surv, col = "green", xlim = c(0,2))
legend("topright",c("original","censured", "Survival_KM"),
       col = c("red", "blue", "green"), lty = 1)
```

![<https://github.com/dlsaavedra/rcens/blob/main/image/Survival_Curve_Example_rcensT3.png>](https://github.com/dlsaavedra/rcens/blob/main/image/Survival_Curve_Example_rcensT3.png)

### Other Examples

In the folder [examples_plot](https://github.com/dlsaavedra/rcens/tree/main/examples_plot) you can find example for each function in this packages.

## Citation

To cite `rcens` in publication use:

Saavedra D, Ramos PL (2024). *rcens: Generate Sample Censoring*. R package version 0.1.0, <https://github.com/dlsaavedra/rcens>.

A BibTeX entry for LaTeX users is:

``` bibtex
  @Manual{,
    title = {rcens: Generate Sample Censoring},
    author = {Daniel Saavedra and Pedro L Ramos},
    year = {2024},
    note = {R package version 0.1.1},
    url = {https://github.com/dlsaavedra/rcens},
  }
```

## Future Work

We are currently working to generate data with interval right censoring, both with fixed and random interval length. Soon we will propose a function to generate data with double interval censoring. All this while maintaining the possibility of using any distribution for the original data.
