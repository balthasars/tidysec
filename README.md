
<!-- README.md is generated from README.Rmd. Please edit that file -->

# {tidysec}

<!-- badges: start -->

<!-- badges: end -->

The goal of {tidysec} is to facilitate working with filings provided by
the American Securities and Exchange Commission. Currently the package
only supports 13F-filings.

## Installation

You can install the development version of {tidysec} from
[Github](https://github.com/balthasars/tidysec) with:

``` r
devtools::install_github("balthasars/tidysec")
```

## Parse a 13F-filing

To parse a 13F-filing you need the link to the filing made to the SEC.
To obtain the links to all filings of a given company use
`get_filings_info()` (not shown here),

``` r
library(tidysec)
snb_q1_2016_13f <- "https://www.sec.gov/Archives/edgar/data/1582202/000158220216000006/InfoTable_Q12016.xml"
get_filing(snb_q1_2016_13f)
#> # A tibble: 2,611 x 10
#>    issuer class cusip value shrsorprnamt sshprnamttype investment_disc…
#>    <chr>  <chr> <chr> <chr> <chr>        <chr>         <chr>           
#>  1 1 800… COM … 6824… 259   34300        SH            SOLE            
#>  2 1ST S… COM   3369… 541   17730        SH            SOLE            
#>  3 2U INC COM   9021… 915   42000        SH            SOLE            
#>  4 3-D S… COM … 8855… 2665  178350       SH            SOLE            
#>  5 3M CO  COM   8857… 2554… 1599604      SH            SOLE            
#>  6 58 CO… SPON… 3168… 5002  89883        SH            SOLE            
#>  7 8POIN… CL A… 2825… 482   33900        SH            SOLE            
#>  8 8X8 I… COM   2829… 1468  151000       SH            SOLE            
#>  9 A10 N… COM   0021… 355   61700        SH            SOLE            
#> 10 AAC H… COM   0003… 352   18300        SH            SOLE            
#> # … with 2,601 more rows, and 3 more variables: voting_authority_sole <chr>,
#> #   voting_authority_shared <chr>, voting_authority_none <chr>
```
