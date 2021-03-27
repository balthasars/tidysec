library(magrittr)
library(tidysec)
cik_blackrock <- "1364742"
br_2020 <- get_13f(cik = cik_blackrock, year = 2020, clean_col_names = FALSE, amendments = FALSE)

cik_random_bank <- c("1535602")
rb_2020 <- get_13f(cik = cik_random_bank, year = 2020, clean_col_names = FALSE, amendments = FALSE)


br_2020 %>%
  # dplyr::filter(quarter %in%) %>%
  tidyr::unnest(filing) %>%
  dplyr::distinct(quarter, date_filed, periodOfReport, reportCalendarOrQuarter, signatureDate)

library(magrittr)
library(tidysec)
cik_random_bank <- c("1535602")
rb_2020 <- get_13f(cik = cik_random_bank, year = 2020, clean_col_names = FALSE, amendments = FALSE)
rb_2020 %>%
  View()

try_with_get_request <- httr::RETRY(verb = "GET", url = "https://www.sec.gov/Archives/edgar/data/1364742/000108636420000038/form13fInfoTable.xml")

"https://www.sec.gov/Archives/edgar/data/1364742/000108636420000038/form13fInfoTable.xml" %>%
  parse_13f_submission_xml()

# temp <- tempfile()
# "https://www.sec.gov/Archives/edgar/data/1364742/000142645920000005/form13fInfoTable.xml" %>%
#   httr::RETRY(verb = "GET", httr::write_disk(path = temp))
# xml2::read_xml(temp)


# <- "/var/folders/bm/3mv2bd6924gc4h_pz36ym8p80000gn/T//Rtmp5kD9az/file6194455372e5sdff"
# retry::retry(expr = sec_meta_xml_raw <- xml2::read_xml(temporary_file_path), when = !exists(temporary_file_path))
