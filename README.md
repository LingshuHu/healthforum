
<!-- README.md is generated from README.Rmd. Please edit that file -->
healthforum <img src='man/figures/logo.png' align="right" height="138.5" />
===========================================================================

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/healthforum)](https://CRAN.R-project.org/package=healthforum) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/mkearney/healthforum.svg?branch=master)](https://travis-ci.org/mkearney/healthforum) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/LingshuHu/healthforum?branch=master&svg=true)](https://ci.appveyor.com/project/LingshuHu/healthforum) <!-- badges: end -->

A package for scraping patientforum discussion threads.

Installation
------------

You can install the released version of healthforum from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("healthforum")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("LingshuHu/healthforum")
```

Example
-------

This is a basic example which shows you how to scrape [this discussion thread from patient.info](https://patient.info/forums/discuss/can-gastritis-be-cured--613999).

``` r
## load healthforum
library(healthforum)

## scrape pages 1-2 from thread about gastritis
gas <- scrape_one_post(
  url = "https://patient.info/forums/discuss/can-gastritis-be-cured--613999",
  From = 1, To = 2)
#> Warning in FUN(X[[i]], ...): NAs introduced by coercion
```

Preview the returned data frame

``` r
tibble::as_tibble(gas)
#> # A tibble: 346 x 13
#>    posts_id post_time           types user_names reply_names likes replies text 
#>  * <chr>    <dttm>              <chr> <chr>      <chr>       <dbl>   <dbl> <chr>
#>  1 613999   2017-09-30 10:38:00 main… TheWolver… <NA>            4     343 I ha…
#>  2 2858159  2017-09-30 14:37:00 reply pippa58442 TheWolveri…     1     332 Gast…
#>  3 2858195  2017-09-30 15:42:00 nest… suzanne_6… pippa58442      0       0 Yes …
#>  4 2858274  2017-09-30 17:56:00 nest… TheWolver… pippa58442      0       0 Will…
#>  5 2858298  2017-09-30 18:27:00 nest… pippa58442 TheWolveri…     1       0 To b…
#>  6 2858300  2017-09-30 18:31:00 nest… TheWolver… pippa58442      0       0 Dont…
#>  7 2858367  2017-09-30 20:22:00 nest… pippa58442 TheWolveri…     0       0 The …
#>  8 2858405  2017-09-30 21:17:00 nest… TheWolver… pippa58442      0       0 HOW …
#>  9 2858502  2017-09-30 23:04:00 nest… pippa58442 TheWolveri…     0       0 I ha…
#> 10 2858730  2017-10-01 08:34:00 nest… TheWolver… <NA>            0       0 I ha…
#> # ... with 336 more rows, and 5 more variables: post_title <chr>, join_date <dttm>,
#> #   posts_num <dbl>, profile_text <chr>, group_names <chr>
```

Disclaimer
----------

`healthforum` was developed to collect publicly available data from the website *patient.info*. The purpose of this package is to facilitate academic research. It is the final user's responsibility to store the data securely and obey all applicable local, state, and federal laws.
