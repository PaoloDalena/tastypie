
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tastypie

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tastypie)](https://CRAN.R-project.org/package=tastypie)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

You only need to type “why pie charts are bad” on Google to find
thousands of articles full of (very valid) reasons why this type of
chart should not be used.  
**However**, my mother, a high school teacher of Latin and Italian
literature, swears she understands more from something like this:

``` r
pie<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) # to be replaced by a function from the package
pie
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

than from something like this:

``` r
kable(df)
```

| group    | value |
| :------- | ----: |
| My mum   |    20 |
| Is       |    30 |
| Strange? |    50 |

Now, I love my mum, and I want her to be happy. And maybe there are
other people in the world in this *situation*, so, why not helping?

Easy pie charts\!  
Just a wrapper for [ggplot2](https://ggplot2.tidyverse.org/) functions
for making pie charts in an easier way.

## Installation

You can install the released version of tastypie from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tastypie")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PaoloDalena/tastypie")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tastypie)
## basic example code
```
