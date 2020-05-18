# helper function for making links to files at all directory levels
construct_link <- function(cik = NULL, level, link_to_dir = NULL) {
  base_url <- "https://www.sec.gov"
  archive_suffix <- "Archives/edgar/data"

  if (level == "links_to_filings") {
    # stop if no CIK
    assertive::assert_is_not_null(cik, severity = "stop")
    # stopifnot(, "CIK must be indicated to retrieve list of a company's filings.")
    # construct link
    paste(base_url, archive_suffix, cik, "index.xml", sep = "/")
  } else if (level == "filing") {
    # stop if no `link_to_dir`
    assertive::assert_is_not_null(link_to_dir, severity = "stop")
    paste0(base_url, link_to_dir, "/", "index.xml")
  } else if(level == "meta_doc"){
    assertive::assert_is_not_null(link_to_dir)
    paste0(base_url, link_to_dir)
  }
}

# test `construct_link()`
# construct_link(cik = cik_snb, level = "links_to_filings")
# construct_link(level = "filing", link_to_dir = "/Archives/edgar/data/1582202/000158220220000002")
# # # faulty call
# try(construct_link(level = "links_to_filings"))


# helper function to `get_filings_info()` and `get_filings()`
get_links_to_filings <- function(cik){

  # access archive
  response <- httr::GET(construct_link(cik = cik, level = "links_to_filings"))
  parsed <- httr::content(response, as = "text", encoding = "UTF-8")

  # # parse links
  links_to_filings <- xml2::read_xml(parsed, "item/href") %>%
    xml2::xml_find_all("item/href") %>%
    xml2::xml_text()

  links <- construct_link(level = "filing", link_to_dir = links_to_filings)

}
# test of `get_links_to_filings()`
# get_links_to_filings(cik = cik_snb) %>% print()

# helper to `get_filing_meta()` and `get_filings()`
get_link_to_files <- function(link) {
  response <- httr::GET(link)
  parsed <- httr::content(response, as = "text", encoding = "UTF-8")

  # list of documents inside filing directory
  docs <- xml2::read_xml(parsed) %>%
    xml2::xml_find_all("item/href") %>%
    xml2::xml_text()

  doc_links_vec <- docs[c(
    grep(pattern = "primary_doc", x = docs),
    grep(pattern = "InfoTable", x = docs)
  )]

  doc_links_df_long <- tibble::tibble(
    doc_link = construct_link(link_to_dir = doc_links_vec, level = "meta_doc"),
    doc_name = c("meta", "filing")
  )

  doc_links_df_wide <- tidyr::pivot_wider(
    data = doc_links_df_long,
    names_from = doc_name,
    values_from = doc_link, names_prefix = "link_")
  doc_links_df_wide
}

# test_gltm <- "https://www.sec.gov/Archives/edgar/data/1582202/000158220216000005/index.xml" %>%
#   get_link_to_files()


# helper function for `get_filing_info()`
xml_find_all_then_text <- function(nodes, xpath){
  xml2::xml_find_all(x = nodes, xpath = xpath) %>%
    xml2::xml_text()
}

# takes input from `get_link_to_files()`
get_filing_meta <- function(link_to_files){

  # read document with information about filing

  # TODO: add checks for HTTP error codes
  # response <- httr::GET(link)
  # parsed <- httr::content(response, as = "text", encoding = "UTF-8")

  meta_doc_xml <- xml2::read_xml(link_to_files$link_meta) %>%
    xml2::xml_ns_strip()

  # meta-information of interest for a given filing
  xpaths <- list(
    submissionType = "//headerData/submissionType",
    cik = "//filer/credentials/cik",
    periodOfReport = "//periodOfReport",
    reportCalendarOrQuarter = "//formData/coverPage/reportCalendarOrQuarter",
    form13FFileNumber = "//formData/coverPage/form13FFileNumber",
    stateOrCountry = "//signatureBlock/stateOrCountry",
    signatureDate = "//signatureBlock/signatureDate",
    tableEntryTotal = "//summaryPage/tableEntryTotal",
    tableValueTotal = "//summaryPage/tableValueTotal"
  )

  # iterate over xpaths and create a wide tibble
  purrr::map_chr(xpaths, xml_find_all_then_text, nodes = meta_doc_xml) %>%
    tibble::enframe() %>%
    tidyr::pivot_wider(names_from = name) %>%
    # add link to meta file and filing
    dplyr::bind_cols(link_to_files)
}
# test
# get_filing_meta(test_gltm)

#' List all filings submitted by company to the SEC.
#'
#' \code{get_filings_info} searches the EDGAR archive for any
#' filings a company has submitted over time.
#'
#' The function returns a tibble with all meta information about all filings,
#' links to the filing itself and the meta information.
#' @param cik Central index key of a company.
#' @param clean_col_names Remove camel case from column names.
#' @examples
#' # Retrieve filings made by the Swiss National Bank
#' cik_snb <- "0001582202"
#' get_filings_info(cik = cik_snb, clean_col_names = TRUE)
#' @export

get_filings_info <- function(cik, clean_col_names = TRUE) {
  message(
    crayon::green("This may take a while..."),
    crayon::green("so drink a"), emo::ji_glue(" :mug: or :tea:"),
    crayon::green("while this finishes.")
  )

  # get links to filings meta
  links <- get_links_to_filings(cik)
  pb <- progress::progress_bar$new(
    total = length(links),
    format = "Getting links to meta filings [:bar] :percent eta: eta"
  )
  links_to_meta <- purrr::map(links, ~ {
    pb$tick()
    get_link_to_files(.x)
  })

  # parse meta information and display progress bar
  pb <- progress::progress_bar$new(
    total = length(links),
    format = "Getting meta for all filings [:bar] :percent eta: [eta]"
  )
  meta_raw <- purrr::map_df(links_to_meta, ~ {
    pb$tick()
    get_filing_meta(.x)
  })

  meta_df <- meta_raw %>%
    # make date columns
    dplyr::mutate_at(vars(periodOfReport, reportCalendarOrQuarter, signatureDate), ~ as.Date(., "%m-%d-%Y"))

  # conditionally clean column names
  if (clean_col_names) {
    meta_df_clean_col_names <- janitor::clean_names(meta_df)
    print(meta_df_clean_col_names)
  } else if (rlang::is_empty(clean_col_names) | isFALSE(clean_col_names)) {
    print(meta_df)
  }
}

#' Retrieve an SEC filing
#'
#' Parses a company's filing
#' @param link_to_filing Link to company filing.
#' @param clean_col_names Remove camel case from column names.
#' @param filing_type Indicate filing type. Only 13F possible, will fail otherwise.
#' @examples
#' snb_q1_2016_13f <- "https://www.sec.gov/Archives/edgar/data/1582202/000158220216000006/InfoTable_Q12016.xml"
#' xml <- get_filing(snb_q1_2016_13f)
#' @export

get_filing <- function(link_to_filing, filing_type = "13F", clean_col_names = TRUE){
  assertive::assert_are_set_equal(filing_type, "13F", severity = "stop")
  # read xml
  xml <- xml2::read_xml(link_to_filing)

  # # be careful here about xpath syntax! good explanation here: https://www.w3schools.com/xml/xpath_syntax.asp
  # # Also, there's a risk of accidentally parsing the children of a specific column,
  # # resulting in the wrong reported values f.e. of the reported shares,
  # # if you do not look at the raw XML file! — f.e. in Firefox:
  # # view-source:https://www.sec.gov/Archives/edgar/data/1582202/000158220220000001/InfoTable_Q42019.xml
  #

  xpaths <- list(
    issuer = "//ns1:nameOfIssuer",
    class = "//ns1:titleOfClass",
    cusip = "//ns1:cusip",
    value = "//ns1:value",
    shrsorprnamt = "//ns1:shrsOrPrnAmt/ns1:sshPrnamt",
    sshprnamttype = "//ns1:shrsOrPrnAmt/ns1:sshPrnamtType",
    investment_discretion = "//ns1:investmentDiscretion",
    voting_authority_sole = "//ns1:votingAuthority/ns1:Sole",
    voting_authority_shared = "//ns1:votingAuthority/ns1:Shared",
    voting_authority_none = "//ns1:votingAuthority/ns1:None"
  )

  # iterate over list of xpaths and make data frame
  filing_df <- purrr::map_df(xpaths, xml_find_all_then_text, nodes = xml)

  # conditionally clean column names
  if (clean_col_names) {
    filing_df_clean_col_names <- janitor::clean_names(filing_df)
    print(filing_df_clean_col_names)
  } else if (rlang::is_empty(clean_col_names) | isFALSE(clean_col_names)) {
    print(filing_df)
  }

}
