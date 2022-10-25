
<!-- README.md is generated from README.Rmd. Please edit that file -->

The goal of this package is to implement analysis scripts and document
calculations and estimations used for the development of a microfluidic
picoinjector.

![Picoinjector](vignettes/images/006_pi30_v4.png)

## Installation

You can install the development version of picoinjector from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fynnwi/picoinjector")
```

## Content

1.  Calculations to estimate hydraulic resistances in PTFE and Tygon
    tubing and the resulting flow rates:
    `vignette("hydraulic_resistance")`
2.  Estimations to quantify the number of droplets contained in x cm of
    tubing: `vignette("droplet_volumes")`
3.  Spin curve analysis of SU-8 3035 custom dilution:
    `vignette("spincurves")`
