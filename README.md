# childfree <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/childfree?color=orange)](https://cran.r-project.org/package=childfree)
[![](http://cranlogs.r-pkg.org/badges/grand-total/childfree?color=blue)](https://cran.r-project.org/package=childfree)
[![](http://cranlogs.r-pkg.org/badges/last-month/childfree?color=green)](https://cran.r-project.org/package=childfree)
[![status](https://tinyverse.netlify.com/badge/childfree)](https://CRAN.R-project.org/package=childfree)
<!-- badges: end -->

## Welcome
Welcome to the `childfree` package\! The childfree package reads demographic data from a variety of public data sources, extracting and harmonizing variables useful for the study of childfree individuals.

The `childfree` package can be cited as:

**Neal, Z. P. and Neal, J. W. (2024). childfree: An R package to access and harmonize childfree demographic data. *GitHub*.**

## Installation
The /release branch ***will eventually contain*** the current CRAN release of the incidentally package. You can install it from [CRAN](https://CRAN.R-project.org) with:
``` r
install.packages("childfree")
```

The /devel branch contains the working beta version of the next release of the childfree package. All the functions are documented and have undergone various levels of preliminary debugging, so they should mostly work, but there are no guarantees. Feel free to use the devel version (with caution), and let us know if you run into any problems. You can install it You can install from GitHub with:
``` r
library(devtools)
install_github("zpneal/childfree", ref = "devel", build_vignettes = TRUE)
```

## Dependencies
The `childfree` package adopts the [tinyverse](https://www.tinyverse.org/) philosophy, and therefore aims to keep dependencies at a minimum.
