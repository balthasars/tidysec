
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{tidysec}`

<!-- badges: start -->
<!-- badges: end -->

The goal of `{tidysec}` is to facilitate working with filings provided
by the American Securities and Exchange Commission. Currently the
package only supports 13F-filings. The package works with the quarterly
master file.

## Installation

You can install the development version of `{tidysec}` from
[Github](https://github.com/balthasars/tidysec) with:

``` r
devtools::install_github("balthasars/tidysec")
```

## Retrieve and parse a 13F-filing

Use `get_13f()` for that purpose. This function returns the filings as a
tibble with a nested data frame column.

``` r
library(tidysec)
library(magrittr)
cik_blackrock <- "1364742"
br_2015 <- get_13f(cik = cik_blackrock, year = 2015, clean_col_names = FALSE, amendments = FALSE)
#> Searching SEC filings from 2015.
#> tidysec caches the SEC's master index files for the time period you've downloaded up to now at/Users/balthasarsager/.rcache/tidysec/ .
#> Run `forget_tidysec_cache()` to delete the master indices if you no longer need them.
#> Now parsing filings...
#> Searching SEC filings from 2015.
#> tidysec caches the SEC's master index files for the time period you've downloaded up to now at/Users/balthasarsager/.rcache/tidysec/ .
#> Run `forget_tidysec_cache()` to delete the master indices if you no longer need them.
#> 👍Those filings are ready now, hehe.
br_2015
#> # A tibble: 4 x 21
#>   cik    company_name  form_type date_filed quarter link_to_filing       filing 
#>   <chr>  <chr>         <chr>          <dbl>   <dbl> <chr>                <list> 
#> 1 13647… BlackRock In… 13F-HR          2015       1 https://www.sec.gov… <tibbl…
#> 2 13647… BlackRock In… 13F-HR          2015       2 https://www.sec.gov… <tibbl…
#> 3 13647… BlackRock In… 13F-HR          2015       3 https://www.sec.gov… <tibbl…
#> 4 13647… BlackRock In… 13F-HR          2015       4 https://www.sec.gov… <tibbl…
#> # … with 14 more variables: filing_number <chr>, submissionType <chr>,
#> #   filingManager <chr>, reportType <chr>, periodOfReport <chr>,
#> #   reportCalendarOrQuarter <chr>, form13FFileNumber <chr>,
#> #   stateOrCountry <chr>, signatureDate <chr>, tableEntryTotal <chr>,
#> #   tableValueTotal <chr>, otherIncludedManagersCount <chr>,
#> #   otherManagers2Info <chr>, link_to_primary_doc <chr>
```

To work with the filing(s), use `tidyr::unnest()`:

``` r
br_2015 %>% 
  tidyr::unnest(filing)
#> # A tibble: 31,298 x 30
#>    cik    company_name form_type date_filed quarter link_to_filing  issuer class
#>    <chr>  <chr>        <chr>          <dbl>   <dbl> <chr>           <chr>  <chr>
#>  1 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 1ST S… COM  
#>  2 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 21VIA… SPON…
#>  3 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3-D S… COM …
#>  4 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3-D S… COM …
#>  5 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3-D S… COM …
#>  6 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3M CO  COM  
#>  7 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3M CO  COM  
#>  8 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3M CO  COM  
#>  9 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3M CO  COM  
#> 10 13647… BlackRock I… 13F-HR          2015       1 https://www.se… 3M CO  COM  
#> # … with 31,288 more rows, and 22 more variables: cusip <chr>, value <dbl>,
#> #   shrsorprnamt <dbl>, sshprnamttype <chr>, investment_discretion <chr>,
#> #   voting_authority_sole <dbl>, voting_authority_shared <dbl>,
#> #   voting_authority_none <dbl>, filing_number <chr>, submissionType <chr>,
#> #   filingManager <chr>, reportType <chr>, periodOfReport <chr>,
#> #   reportCalendarOrQuarter <chr>, form13FFileNumber <chr>,
#> #   stateOrCountry <chr>, signatureDate <chr>, tableEntryTotal <chr>,
#> #   tableValueTotal <chr>, otherIncludedManagersCount <chr>,
#> #   otherManagers2Info <chr>, link_to_primary_doc <chr>
```

Amendments to 13F filings (13F-HR/A) cannot be parsed yet. However, you
can still get the link to their .txt file by setting `link_only = TRUE`
and `amendments = TRUE`:

``` r
cik_blackrock <- "1364742"
get_13f(cik = cik_blackrock, year = 2015, link_only = TRUE, amendments = TRUE)
#> Searching SEC filings from 2015.
#> tidysec caches the SEC's master index files for the time period you've downloaded up to now at/Users/balthasarsager/.rcache/tidysec/ .
#> Run `forget_tidysec_cache()` to delete the master indices if you no longer need them.
#> # A tibble: 5 x 6
#>   cik    company_name  form_type date_filed quarter link_to_filing              
#>   <chr>  <chr>         <chr>          <dbl>   <dbl> <chr>                       
#> 1 13647… BlackRock In… 13F-HR          2015       1 https://www.sec.gov/Archive…
#> 2 13647… BlackRock In… 13F-HR          2015       2 https://www.sec.gov/Archive…
#> 3 13647… BlackRock In… 13F-HR          2015       3 https://www.sec.gov/Archive…
#> 4 13647… BlackRock In… 13F-HR/A        2015       4 https://www.sec.gov/Archive…
#> 5 13647… BlackRock In… 13F-HR          2015       4 https://www.sec.gov/Archive…
```

## Get a list of all SEC filings by a company

Let’s say you wanted a list of all of Credit Suisse AG’s 2020 company
filings.

``` r
cik_credit_suisse <- "824468"
cs_2019 <- get_list_of_filings(
  cik = cik_credit_suisse, 
  year = 2020, 
  filing_type = "all",
  clean_col_names = TRUE 
  )
#> Searching SEC filings from 2020.
#> tidysec caches the SEC's master index files for the time period you've downloaded up to now at/Users/balthasarsager/.rcache/tidysec/ .
#> Run `forget_tidysec_cache()` to delete the master indices if you no longer need them.
cs_2019
#> # A tibble: 16 x 6
#>    cik    company_name    form_type date_filed filename                  quarter
#>    <chr>  <chr>           <chr>          <dbl> <chr>                       <dbl>
#>  1 824468 CREDIT SUISSE … 13F-HR          2020 edgar/data/824468/000156…       1
#>  2 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  3 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  4 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  5 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  6 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  7 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  8 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#>  9 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#> 10 824468 CREDIT SUISSE … SC 13G/A        2020 edgar/data/824468/000156…       1
#> 11 824468 CREDIT SUISSE … SC 13G          2020 edgar/data/824468/000156…       1
#> 12 824468 CREDIT SUISSE … SC 13G          2020 edgar/data/824468/000156…       1
#> 13 824468 CREDIT SUISSE … SC 13G          2020 edgar/data/824468/000156…       1
#> 14 824468 CREDIT SUISSE … 13F-HR          2020 edgar/data/824468/000156…       2
#> 15 824468 CREDIT SUISSE … 13F-HR          2020 edgar/data/824468/000156…       3
#> 16 824468 CREDIT SUISSE … 13F-HR          2020 edgar/data/824468/000156…       4
```

# Other Resources to Work with SEC Data

## R packages

The following packages may also be helpful to you if you’re looking to
work with anything else than 13F filings (I haven’t tested them though):

-   [`{finreportr}`](https://github.com/sewardlee337/finreportr) and/or
    [`{finstr}`](https://github.com/bergant/finstr)
    -   for accessing financial statements of companies
-   [`{edgarWebR}`](https://github.com/mwaldstein/edgarWebR)
    -   with an emphasis on finding various kinds of filings
-   [`{edgar}`](https://cran.r-project.org/web/packages/edgar/) for a
    variety of functions

## APIs

If you just want easy acess to filings, check out finance APIs that
offer 13F filings:

-   [FMP Cloud](https://fmpcloud.io)
