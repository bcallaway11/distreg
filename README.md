
<!-- README.md is generated from README.Rmd. Please edit that file -->
TempleMetrics
=============

The TempleMetrics package is a collection of functions implemented by members of the Econometrics Reading Group at Temple University. The main functions (at the moment) are built for distribution regression. That is, one can estimate the distribution of \(Y\) conditional on \(X\) using a model for a binary outcome. For example,
\begin{align*}
  F_{Y|X}(y|x) = \Lambda(x'\beta)
\end{align*}
where \(\Lambda\) is some known link function, such as logit.

Installation
------------

You can install TempleMetrics from github with:

``` r
# install.packages("devtools")
devtools::install_github("bcallaway11/TempleMetrics")
```

or from CRAN using

``` r
install.packages("TempleMetrics")
```

Example 1
---------

The first example is how to run distribution regression for a single value of \(y\) of \(Y\). This example uses the `igm` dataset which is a collection of 500 parent-child pairs of income along with the parent's education level which comes from the Panel Study of Income Dynamics (PSID).

``` r
library(TempleMetrics)
data(igm)
head(igm)
#>      lcfincome lfincome  HEDUC
#> 1943 11.599712 11.46909     HS
#> 823  11.396635 10.71540     HS
#> 597  11.805380 10.87307     HS
#> 1291 11.007568 11.45044     HS
#> 3333 10.915150 11.30180     HS
#> 2717  9.764008 10.80891 LessHS
```

``` r
y0 <- median(igm$lcfincome)
distreg
#> function (yvals, data, yname, xnames) 
#> {
#>     DR(yvals, drs(yvals, data, yname, xnames))
#> }
#> <environment: namespace:TempleMetrics>
```
