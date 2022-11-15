
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crtcr: Convergent Recombination of TCR Repertoire

<!-- badges: start -->
<!-- badges: end -->

A commonly used method for quantifying the degree of similarity of TCR
repertoires is the concept of convergent recombination of T-cell
receptors (TCRs).

This R package implements this method and includes key functions that
are involved throughout the execution of such analyses.

On the basis of these supplied measurements calculations, it is possible
to identify identical clonotypes that resulted from a range of clonal
sequences, which may then be used for an analysis method that
corresponds with a particular immunological state.

## Installation

You can install the released version of crtcr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("crtcr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("idohasson/crtcr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(crtcr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
