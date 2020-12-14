
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distreg

The `distreg` package is a collection of functions built for
distribution regression. That is, one can estimate the distribution of
\(Y\) conditional on \(X\) using a model for a binary outcome. For
example, \[
  F_{Y|X}(y|x) = \Lambda(x'\beta)
\] where \(\Lambda\) is some known link function, such as logit.

This package was formerly called `TempleMetrics`. For the time being, we
are just hosting the package on Github, but it should work just fine for
you. The main methods are:

  - `distreg` – a function to implement distribution regression as in
    Foresi and Peracchi (1995) <doi:10.2307/2291056> and Chernozhukov,
    Fernandez-Val, and Melly (2013) <doi:10.3982/ECTA10582>.

  - `lldistreg` – a function to implement a local linear regression
    (it’s local in just one particular variable – often a
    \`\`treatment’’)

In either case, it is often useful to convert these estimates into
estimates of conditional distributions themselves. This can be done with
the function `Fycondx`. We give some examples below.

## Installation

You can install `distreg` from github with:

``` r
# install.packages("devtools")
devtools::install_github("bcallaway11/distreg")
#> Downloading GitHub repo bcallaway11/distreg@HEAD
#> 
#>      checking for file ‘/tmp/RtmpiH5t3P/remotesd7327af16bb8/bcallaway11-distreg-ff2f263/DESCRIPTION’ ...  ✔  checking for file ‘/tmp/RtmpiH5t3P/remotesd7327af16bb8/bcallaway11-distreg-ff2f263/DESCRIPTION’
#>   ─  preparing ‘distreg’:
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘distreg_1.3.0.tar.gz’
#>      
#> 
#> Installing package into '/tmp/RtmpTFpvLA/temp_libpathc85b78baa01d'
#> (as 'lib' is unspecified)
```

## Example 1

The first example is how to run distribution regression for a single
value of \(y\) of \(Y\). This example uses the `igm` dataset which is a
collection of 500 parent-child pairs of income along with the parent’s
education level which comes from the Panel Study of Income Dynamics
(PSID).

``` r
library(distreg)
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
dreg <- distreg(lcfincome ~ lfincome + HEDUC, igm, y0)
dreg
#> $yvals
#> [1] 11.04563
#> 
#> $glmlist
#> $glmlist[[1]]
#> 
#> Call:  glm(formula = formla, family = binomial(link = link), data = dta)
#> 
#> Coefficients:
#> (Intercept)     lfincome      HEDUCHS  HEDUCLessHS  
#>     15.3320      -1.3976       0.1629       0.2256  
#> 
#> Degrees of Freedom: 499 Total (i.e. Null);  496 Residual
#> Null Deviance:       693.1 
#> Residual Deviance: 640.9     AIC: 648.9
#> 
#> 
#> attr(,"class")
#> [1] "DR"
```

## Example 2

In many cases, of primary interest with distribution regression is
obtaining \(\hat{F}_{Y|X}(y|x)\) for some particular values of \(y\) and
\(x\). That’s what we do in this example.

``` r
yvals <- seq(quantile(igm$lcfincome,.05,type=1),
quantile(igm$lcfincome,.95, type=1), length.out=100)
dres <- distreg(lcfincome ~ lfincome + HEDUC, igm, yvals)
xdf <- data.frame(lfincome=10, HEDUC="LessHS")
y0 <- yvals[50]
ecdf(igm$lcfincome)(y0)
#> [1] 0.328
Fycondx(dres, y0, xdf)
#> [[1]]
#> Empirical CDF 
#> Call: NULL
#>  x[1:100] = 9.6856, 9.7073, 9.7291,  ..., 11.814, 11.836
```

This example says that: (1) the fraction of “children” in the dataset
with income below 46628 is 0.33, but (2) we estimate that the fraction
of children whose parent’s income is 22026 and have parent’s with less
than a HS education with income below 46628 is 0.73.
