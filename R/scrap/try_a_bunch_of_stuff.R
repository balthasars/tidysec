library(tidysec)
library(magrittr)


filing_list <- get_list_of_filings(cik = ciks, year = 2021, filing_type = c("13F-HR", "13F-HR/A"))

# welche nicht vorhanden?
setdiff(ciks, filing_list$cik)

filing_list <- get_13f(cik = ciks, year = 2021, filing_type = c("13F-HR", "13F-HR/A"))


sample_data <- tidysec:::parse_13f_submission_xml("https://www.sec.gov/Archives/edgar/data/1132716/000113271621000005/ubsocQ4_20_A1infotable.xml")

filing_proto <- tibble::tibble(
  issuer = NA_character_,
  class = NA_character_,
  cusip = NA_character_,
  value = NA_integer_,
  shrsorprnamt = NA_integer_,
  sshprnamttype = NA_character_,
  investment_discretion = NA_character_,
  voting_authority_sole = NA_integer_,
  voting_authority_shared = NA_integer_,
  voting_authority_none = NA_integer_
)

library(tidysec)
ciks <- c(
  1588340,1634222,928633,1368163,1535602, 1291274,1599469,
  1599576,1535631,1793755,1544599,1068833,1727588, 824468,
  1321482, 903954,861177,1132716, 1610520,1114446,1615423,
  1535660,1535784,1491719,1528147,1615423,1615424,1615305,
  1649647,1689918,1658354,1667654,1649591,1650162,1649592,
  1536550,1641992
)
problematic_ciks <- c(1321482, 903954,861177,1132716, 1610520,1114446,1615423)

library(magrittr)
try_again <- get_13f(cik = ciks, year = 2021, amendments = TRUE, clean_col_names = FALSE)

try_again %>%
  tidyr::unnest(filing) %>%
  janitor::clean_names()

working_example %>%
  dplyr::select(filing) %>%
  tidyr::unnest(filing)

tidysec::get_list_of_filings(cik = 1641992, year = 2021)

# parse_13f_submission doesn't work for quite a few XML files
"https://www.sec.gov/Archives/edgar/data/1641992/000106299321000400/form13fInfoTable.xml" %>%
  tidysec:::parse_13f_submission_xml_2()
  xml2::read_xml() %>%
  xml2::xml_find_all("n1:infoTable/n1:nameOfIssuer")

library(dplyr)
working_example %>%
  select(-filing) %>%
  mutate_if(is.numeric, ~NA_integer_) %>%
  mutate_if(is.character, ~NA_character_) %>%
  datapasta::df_paste()


example_for_failing <- get_13f(cik = 1321482, year = 2021, amendments = TRUE, clean_col_names = TRUE)
example_for_failing

get_list_of_filings(cik = 1321482, year = 2021) %>%
  nrow()
