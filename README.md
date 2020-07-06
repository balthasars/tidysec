
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
#> Getting Q1 master index from https://www.sec.gov/Archives/edgar/full-index/2015/QTR1/master.idx
#> Getting Q2 master index from https://www.sec.gov/Archives/edgar/full-index/2015/QTR2/master.idx
#> Getting Q3 master index from https://www.sec.gov/Archives/edgar/full-index/2015/QTR3/master.idx
#> Getting Q4 master index from https://www.sec.gov/Archives/edgar/full-index/2015/QTR4/master.idx
#> Now parsing filings...
#> 👍Those filings are ready now, hehe.
br_2015
#> # A tibble: 4 x 7
#>   cik    company_name  form_type date_filed quarter link_to_filing       filing 
#>   <chr>  <chr>         <chr>          <dbl>   <dbl> <chr>                <list> 
#> 1 13647… BlackRock In… 13F-HR          2015       1 https://www.sec.gov… <tibbl…
#> 2 13647… BlackRock In… 13F-HR          2015       2 https://www.sec.gov… <tibbl…
#> 3 13647… BlackRock In… 13F-HR          2015       3 https://www.sec.gov… <tibbl…
#> 4 13647… BlackRock In… 13F-HR          2015       4 https://www.sec.gov… <tibbl…
```

To work with the filing(s), use `tidyr::unnest()`:

``` r
br_2015 %>% 
  tidyr::unnest(filing)
#> # A tibble: 31,298 x 16
#>    cik   company_name form_type date_filed quarter link_to_filing issuer class
#>    <chr> <chr>        <chr>          <dbl>   <dbl> <chr>          <chr>  <chr>
#>  1 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 1ST S… COM  
#>  2 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 21VIA… SPON…
#>  3 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3-D S… COM …
#>  4 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3-D S… COM …
#>  5 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3-D S… COM …
#>  6 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3M CO  COM  
#>  7 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3M CO  COM  
#>  8 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3M CO  COM  
#>  9 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3M CO  COM  
#> 10 1364… BlackRock I… 13F-HR          2015       1 https://www.s… 3M CO  COM  
#> # … with 31,288 more rows, and 8 more variables: cusip <chr>, value <dbl>,
#> #   shrsorprnamt <dbl>, sshprnamttype <chr>, investment_discretion <chr>,
#> #   voting_authority_sole <dbl>, voting_authority_shared <dbl>,
#> #   voting_authority_none <dbl>
```

Amendments to 13F filings (13F-HR/A) cannot be parsed yet. However, you
can still get the link to their .txt file by setting `link_only = TRUE`
and `amendments = TRUE`:

``` r
cik_blackrock <- "1364742"
get_13f(cik = cik_blackrock, year = 2015, link_only = TRUE, amendments = TRUE)
#> Warning in if (filing_type == "all") {: the condition has length > 1 and only
#> the first element will be used
#> Searching SEC filings from 2015.
#> tidysec caches the SEC's master index files for the time period you've downloaded up to now at/Users/balthasarsager/.rcache/tidysec/ .
#> Run `forget_tidysec_cache()` to delete the master indices if you no longer need them.
#> Getting Q1 master index from https://www.sec.gov/Archives/edgar/full-index/2015/QTR1/master.idx
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
#> Getting Q1 master index from https://www.sec.gov/Archives/edgar/full-index/2020/QTR1/master.idx
#> Getting Q2 master index from https://www.sec.gov/Archives/edgar/full-index/2020/QTR2/master.idx
#> Getting Q3 master index from https://www.sec.gov/Archives/edgar/full-index/2020/QTR3/master.idx
cs_2019
#> # A tibble: 14 x 6
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
```
