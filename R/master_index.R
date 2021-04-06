# link to SEC archive (full index)
link_to_archives <- "https://www.sec.gov/Archives/edgar/full-index"

# `get_check_parse_xml()` ensures HTTP response codes equal 200
get_check_parse_xml <- function(url) {
  response <- httr::RETRY(url = url, verb = "GET")
  # check if status code is fine
  assertive::assert_are_set_equal(httr::status_code(response), 200, severity = "stop")
  parsed <- httr::content(response, as = "text", encoding = "UTF-8")

  xml2::read_xml(parsed)
}

xml_find_all_then_text <- function(nodes, xpath) {
  xml2::xml_find_all(x = nodes, xpath = xpath) %>%
    xml2::xml_text()
}

construct_links_to_all_quarterly_master_indices_for_a_given_year <- function(year) {
  xml <- "index.xml"
  link_to_quarter_dir <- paste(link_to_archives, year, xml, sep = "/")

  quarters_dir_xml <- get_check_parse_xml(url = link_to_quarter_dir)
  quarters_text <- quarters_dir_xml %>%
    xml2::xml_find_all("//directory/item/name") %>%
    xml2::xml_text()

  paste(link_to_archives, year, quarters_text, "master.idx", sep = "/")
}


get_quarter_number_from_link <- function(x) {
  stringr::str_extract(x, "QTR[0-9]+") %>%
    readr::parse_number()
}

download_single_master_index_file <- function(link) {
  quarter_number <- get_quarter_number_from_link(link)

  # construct download message
  message(crayon::cyan(glue::glue(
    "Getting Q",
    quarter_number,
    " master index ",
    "from ",
    {
      link
    }
  )))

  response <- httr::RETRY(url = link, verb = "GET")

  # check if status code is fine
  assertive::assert_are_set_equal(httr::status_code(response), 200, severity = "stop")

  content <- httr::content(response, as = "raw")


  master_index <- vroom::vroom(
    file = content,
    skip = 11, delim = "|",
    col_names = c("cik", "company_name", "form_type", "date_filed", "filename"),
    col_types = c("cccnc")
  ) %>%
    tibble::add_column(quarter = quarter_number)

  # Sys.sleep(3)
  master_index
}

# set cache
path_cache_fs <- "~/.rcache/tidysec/"
cache_fs <- memoise::cache_filesystem(path_cache_fs)
# memoised function `download_single_master_index_file()` for continued usage
mem_download_single_master_index_file <- memoise::memoise(download_single_master_index_file, cache = cache_fs)

#' Clear tidysec cache of master index files.
#'
#' \code{forget_tidysec_cache} clears the cache
#' of master index files in which the links to all filings are
#' document.

#' @export
forget_tidysec_cache <- function() {
  result <- memoise::forget(mem_download_single_master_index_file)
  if (result) {
    message("tidysec cache of master indices deleted.")
  }
}

get_master_indices_for_single_year <- function(year = NULL) {
  message(crayon::blue(glue::glue(
    "Searching SEC filings from ",
    {
      year
    },
    "."
  )))

  # stop if year argument is null
  assertive::assert_is_not_null(year)

  # generate links to master files
  master_index_links <- purrr::map(year, construct_links_to_all_quarterly_master_indices_for_a_given_year) %>%
    purrr::as_vector()

  # message about caching
  message(crayon::red(glue::glue(
    "tidysec caches the SEC's master index files for the time period you've downloaded up to now at",
    {
      path.expand(path_cache_fs)
    },
    " .
      ",
    "Run `forget_tidysec_cache()` to delete the master indices if you no longer need them."
  )))

  # download master indices for years in input
  master_index_lst <- purrr::map(master_index_links, mem_download_single_master_index_file)
  # bind data frames together
  master_index_dt <- data.table::rbindlist(master_index_lst)
  master_index_dt
}

# helper for `subset_master_indices()`
# takes input from `subset_master_indices()`
construct_link_to_filing_directory <- function(dt, xml_index = TRUE) {
  file_names_without_ext <- basename(tools::file_path_sans_ext(dt$filename))
  # remove dashes to construct last part of filing directory
  last_part_of_dir_path <- stringr::str_remove_all(file_names_without_ext, "-")

  cik_vec <- dt$cik

  archives_data_url <- "https://www.sec.gov/Archives/edgar/data"
  xml <- "index.xml"

  if (xml_index) {
    paste(archives_data_url, cik_vec, last_part_of_dir_path, xml, sep = "/")
  } else if (!xml_index) {
    paste(archives_data_url, cik_vec, last_part_of_dir_path, sep = "/")
  }
}

# takes input from `get_master_indices_for_single_year()`
subset_master_indices <- function(master_index_dt, form_type_in = NULL, cik_company) {
  assertive::assert_is_not_null(cik_company, severity = "stop")
  assertive::assert_is_not_null(master_index_dt, severity = "stop")

  # browser()

  # conditionally subset filings depending on function arguments
  if (is.null(form_type_in)) {
    links <- master_index_dt %>%
      dplyr::filter(cik %in% cik_company)
  } else {
    # browser()
    # must be `&` not `,` to concatenate conditions!
    links <- master_index_dt %>%
      dplyr::filter(form_type %in% form_type_in & cik %in% cik_company)
  }

  # # TODO: stop execution in smart way when this fails, no mere `stop()`
  if (rlang::is_empty(links)) {
    message(crayon::red(
      "No filings found. Did you correctly specify the CIK (maybe check for excess leading zeroes) and the filing type?
    Tip: The company you are looking for be may be filing under a different CIK due to corporate restructuring."
    ))
  }
  return(links)
}

# get_master_indices_for_single_year(2014) %>%
#   subset_master_indices(form_type_in = NULL, cik_company = "1364742")
# .[form_type %in% "13F-HR"]

# test_df <- get_master_indices_for_single_year(2020) %>%
#   subset_master_indices(form_type_in = c("13F-HR", "13F-HR/A"), cik_company = "1649647")
# test_df %>%
#   get_13f_link_test()

# helper for `get_13f_link()`
# search for xml info table file.
# returns a `.txt`-file if no XML-file is available.
check_for_and_get_xml_infotable_file <- function(link_to_filing_dir, link_to_filing_txt) {
  file_names <- get_check_parse_xml(
    link_to_filing_dir
  ) %>%
    xml2::xml_find_all("item/name") %>%
    xml2::xml_text()

  file_names

  # result_of_check <- grep(pattern = "InfoTable.*xml", x = file_names)
  result_of_check <- file_names %>%
    .[endsWith(x = ., "xml")] %>%
    .[stringr::str_detect(string = ., pattern = "primary_doc", negate = TRUE)]

  # if there a regex match for `InfoTable.xml` in the filenames,
  # subset to that position in the vector, otherwise
  if (!rlang::is_empty(result_of_check)) {
    # file_names[result_of_check]
    result_of_check
  } else {
    link_to_filing_txt
  }
}

# helper for `get_13f_meta_link()`
# search for xml primary_doc file.
check_for_and_get_xml_primary_doc_file <- function(link_to_filing_dir, link_to_filing_txt) {
  file_names <- get_check_parse_xml(link_to_filing_dir) %>%
    xml2::xml_find_all("item/name") %>%
    xml2::xml_text()

  result_of_check <- file_names %>%
    .[stringr::str_detect(string = ., pattern = "primary_doc")]

  # if there a regex match for `primary_doc` in the filenames,
  # subset to that position in the vector, otherwise
  if (!rlang::is_empty(result_of_check)) {
    paste0(dirname(link_to_filing_dir), "/", result_of_check)
  } else {
    NULL
  }
}

# test_df <- get_master_indices_for_single_year(2020) %>%
#   subset_master_indices(form_type_in = c("13F-HR", "13F-HR/A"), cik_company = "1615423")


# test_df %>%
#   get_13f_link_test()

# test new version
## "https://www.sec.gov/Archives/edgar/data/1615423/000142050620001209/index.xml" %>%
## "https://www.sec.gov/Archives/edgar/data/1535602/000153560220000005/index.xml" %>%
## "https://www.sec.gov/Archives/edgar/data/1114446/000095012314011529/index.xml" %>%
# "https://www.sec.gov/Archives/edgar/data/1544599/000154459920000007/index.xml" %>%
#   check_for_and_get_xml_primary_doc_file()
# .[endsWith(x = ., "xml")] %>%
# .[stringr::str_detect(string = ., pattern = "primary_doc", negate = TRUE)]



construct_path_to_13f_filing <- function(link_to_filing, link_to_filing_dir) {
  dplyr::if_else(
    base::endsWith(link_to_filing, "xml"),
    # what to do with XML files
    paste(stringr::str_remove_all(link_to_filing_dir, "/index.xml"), link_to_filing, sep = "/"),
    # what to do with txt files
    paste("https://www.sec.gov/Archives", link_to_filing, sep = "/")
  )
}


# takes input from `subset_master_indices()`
get_13f_link <- function(list_of_all_filings, forms) {

  # subset to 13F filings
  # list_of_13f_filings <- list_of_all_filings[form_type %in% c("13F-HR/A", "13F-HR")]
  # list_of_13f_filings <- list_of_all_filings[form_type %in% forms]

  # construct links to directories containing filings
  links_to_filing_dirs <- list_of_all_filings %>%
    dplyr::mutate(link_to_filing_dir = construct_link_to_filing_directory(
      dt = list_of_all_filings, xml_index = TRUE
    ))

  # Get links to `InfoTable` files if any XML documents are found in
  # a filing directory using `check_for_and_get_xml_infotable_file()`.
  # Otherwise, a txt file is returned.

  list_of_filings_plus_xml_link <- links_to_filing_dirs %>%
    dplyr::mutate(link_to_filing = purrr::map2_chr(
      link_to_filing_dir, filename,
      check_for_and_get_xml_infotable_file
    ))

  # Construct full path to XML files with filings
  full_path_links <- list_of_filings_plus_xml_link %>%
    dplyr::mutate(link_to_filing = construct_path_to_13f_filing(link_to_filing, link_to_filing_dir)) %>%
    dplyr::select(-c(filename, link_to_filing_dir))

  full_path_links
}

get_13f_meta_link <- function(list_of_all_filings, forms) {

  # construct links to directories containing filings
  links_to_filing_dirs <- list_of_all_filings %>%
    dplyr::mutate(link_to_filing_dir = construct_link_to_filing_directory(
      dt = list_of_all_filings, xml_index = TRUE
    ))

  # Get links to `InfoTable` files if any XML documents are found in
  # a filing directory using `check_for_and_get_xml_infotable_file()`.
  # Otherwise, a txt file is returned.
  list_of_filings_plus_xml_link <- links_to_filing_dirs %>%
    dplyr::mutate(link_to_filing = purrr::map2_chr(
      link_to_filing_dir, filename,
      check_for_and_get_xml_primary_doc_file
    ))

  print(list_of_filings_plus_xml_link)
  #
  # Construct full path to XML files with filings
  full_path_links <- list_of_filings_plus_xml_link %>%
    dplyr::mutate(link_to_filing = construct_path_to_13f_filing(link_to_filing, link_to_filing_dir)) %>%
    dplyr::select(-c(filename, link_to_filing_dir))

  full_path_links
}


meta_proto_tbl <- tibble::tibble(
  cik = NA_character_,
  company_name = NA_character_,
  form_type = NA_character_,
  date_filed = NA_character_,
  quarter = NA_integer_,
  link_to_filing = NA_character_,
  filing_number = NA_character_,
  submissionType = NA_character_,
  filingManager = NA_character_,
  reportType = NA_character_,
  periodOfReport = NA_character_,
  reportCalendarOrQuarter = NA_character_,
  form13FFileNumber = NA_character_,
  stateOrCountry = NA_character_,
  signatureDate = NA_character_,
  tableEntryTotal = NA_integer_,
  tableValueTotal = NA_integer_,
  otherIncludedManagersCount = NA_character_,
  otherManagers2Info = NA_character_,
  link_to_primary_doc = NA_character_
)

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

#' Retrieve a list of a company's SEC filings.
#'
#' \code{get_list_of_filings} searches the EDGAR archive for any
#' filings a company has submitted in a given year.
#'
#' The function returns a tibble with the .txt links to a company's filings,
#' some meta information for said filings itself and the meta information.
#' @param cik Central index key of a company.
#' @param filing_type Type of filing, defaults to all.
#' @param year Which year or time period of filings.
#' @examples
#' # Retrieve all filings made by the Swiss National Bank in 2013.
#' swiss_national_bank_cik <- "1582202"
#' get_list_of_filings(cik = swiss_national_bank_cik, 2013)
#' # Retrieve meta information about 13F-HR but not 13F-HR/A filings by BlackRock and UBS from 2014 to 2016.
#' ubs_cik <- "1114446"
#' blackrock_cik <- "1364742"
#' get_list_of_filings(cik = c(blackrock_cik, ubs_cik), year = 2014:2016, filing_type = "13F-HR")
#' @export

get_list_of_filings <- function(cik, year, filing_type = "all") {

  # conditionally replace `filing_type` argument for helper functions
  if (any(filing_type %in% "all")) {
    filing_type_arg <- NULL
  } else {
    filing_type_arg <- filing_type
  }

  master_indices <- purrr::map_df(year, get_master_indices_for_single_year)
  # browser()
  subset_list <- subset_master_indices(
    master_index_dt = master_indices,
    cik_company = cik,
    form_type_in = filing_type_arg
  ) %>%
    dplyr::as_tibble()

  # conditionally print message if filings not find for any CIKs
  overlap_cik_input_data <- setdiff(cik, subset_list$cik)

  if (!rlang::is_empty(overlap_cik_input_data)) {
    print(
      paste(
        "No filings found for CIK",
        paste(setdiff(cik, subset_list$cik), collapse = ",")
      )
    )
  }

  # ∆ TODO: return prototype of data instead!
  # - indicator to 13F that empty prototype should be returned instead?
  # -
  # abort if no data found
  if (nrow(subset_list) == 0) {
    rlang::abort(message = "No data found for CIK.")
  }

  return(subset_list)
}

# cik_ubs <- "1114446"
# cik_snb <- "1582202"
# get_list_of_filings(cik = cik_ubs, year = 2013,
# ubs_filings <- get_list_of_filings(cik = cik_ubs, year = 2013:2016,
# ubs_filings %>% count(form_type, sort = TRUE) %>% as_tibble()

# functions to parse the filing

# GET request to SEC server, memoised
ua <- httr::user_agent("https://github.com/balthasars/tidysec")

# get namespace of 13F XML docs
get_13f_ns <- function(xml) {
  xml2::xml_ns(xml) %>%
    tibble::enframe() %>%
    dplyr::filter(stringr::str_detect(value, "table")) %>%
    dplyr::sample_n(1) %>%
    dplyr::pull(name)
}

parse_13f_submission_xml <- function(xml_link) {
  # read xml and check for status
  xml_raw <- get_check_parse_xml(xml_link)

  # get name space
  ns <- get_13f_ns(xml_raw)

  # get children of
  info_tables <- xml_raw %>%
    xml2::xml_find_all(
      xpath = paste0("//", ns, ":", "infoTable")
    )

  get_xml_values <- function(x) {
    # add namespace and `:` in front of xpath below;
    # in front of nodes
    xpath <- paste0(ns, ":", x) %>%
      # second node
      gsub(pattern = "/", replacement = paste0("/", ns, ":"), fixed = TRUE)

    info_tables %>%
      xml2::xml_find_all(xpath) %>%
      xml2::xml_text()
  }

  # # be careful here about xpath syntax! good explanation here: https://www.w3schools.com/xml/xpath_syntax.asp
  # # Also, there's a risk of accidentally parsing the children of a specific column,
  # # resulting in the wrong reported values f.e. of the reported shares,
  # # if you do not look at the raw XML file! — f.e. in Firefox:
  # # view-source:https://www.sec.gov/Archives/edgar/data/1582202/000158220220000001/InfoTable_Q42019.xml

  parse_raw_xml <- function() {
    positions <- tibble::tibble(
      issuer = get_xml_values("nameOfIssuer"),
      class = get_xml_values("titleOfClass"),
      cusip = get_xml_values("cusip"),
      value = get_xml_values("value"),
      shrsorprnamt = get_xml_values("shrsOrPrnAmt/sshPrnamt"),
      sshprnamttype = get_xml_values("shrsOrPrnAmt/sshPrnamtType"),
      investment_discretion = get_xml_values("investmentDiscretion"),
      voting_authority_sole = get_xml_values("votingAuthority/Sole"),
      voting_authority_shared = get_xml_values("votingAuthority/Shared"),
      voting_authority_none = get_xml_values("votingAuthority/None")
    ) %>%
      dplyr::mutate(dplyr::across(c(value, shrsorprnamt, tidyselect::starts_with("voting")), as.numeric))
    positions
  }
  positions_output <- parse_raw_xml()

  positions_output
}

parse_13f_meta_xml <- function(link_to_primary_doc) {

  # read document with information about filing
  meta_doc_xml <- get_check_parse_xml(url = link_to_primary_doc)
  xml2::xml_ns_strip(meta_doc_xml)

  # meta-information of interest for a given filing
  xpaths <- list(
    submissionType = "//headerData/submissionType",
    cik = "//filer/credentials/cik",
    filingManager = "//formData/coverPage/filingManager/name",
    reportType = "//formData/coverPage/reportType",
    periodOfReport = "//periodOfReport",
    reportCalendarOrQuarter = "//formData/coverPage/reportCalendarOrQuarter",
    form13FFileNumber = "//formData/coverPage/form13FFileNumber",
    stateOrCountry = "//signatureBlock/stateOrCountry",
    signatureDate = "//signatureBlock/signatureDate",
    tableEntryTotal = "//summaryPage/tableEntryTotal",
    tableValueTotal = "//summaryPage/tableValueTotal",
    otherIncludedManagersCount = "//summaryPage/otherIncludedManagersCount",
    coverPage = "//coverPage/reportCalendarOrQuarter",
    isAmendment = "//coverPage/isAmendment",
    amendmentNo = "//coverPage/amendmentNo",
    amendmentType = "//coverPage/amendmentInfo"
  )

  # more meta information on the filings
  xpaths_other_managers_included <- list(
    otherManagerName = "//summaryPage/otherManagers2Info/otherManager2/otherManager/name",
    otherManagerForm13FFileNumber = "//summaryPage/otherManagers2Info/otherManager2/otherManager/form13FFileNumber",
    otherManagersInfootherManagerform13FFileNumber = "//coverPage/otherManagersInfo/otherManager/form13FFileNumber",
    otherManagersInfootherManagerform13Fname = "//coverPage/otherManagersInfo/otherManager/name"
  )

  # iterate over xpaths and create a wide tibble
  other_man <- purrr::map(xpaths_other_managers_included, xml_find_all_then_text, nodes = meta_doc_xml) %>%
    tibble::enframe() %>%
    tidyr::pivot_wider() %>%
    dplyr::rename(
      other_included_managers_file_number = otherManagerName,
      other_included_managers_name = otherManagerName,
      other_managers_reporting_for_this_manager_file_number = otherManagersInfootherManagerform13FFileNumber,
      other_managers_reporting_for_this_manager = otherManagersInfootherManagerform13Fname
    )
  # iterate over xpaths and create a wide tibble
  base <- purrr::map(xpaths, xml_find_all_then_text, nodes = meta_doc_xml) %>%
    tibble::enframe() %>%
    tidyr::unnest(value) %>%
    tidyr::pivot_wider() %>%
    # add link to primary doc
    dplyr::mutate(link_to_primary_doc = link_to_primary_doc) %>%
    # add filing number to join with actual filing
    dplyr::mutate(filing_number = dirname(link_to_primary_doc) %>% basename())

  dplyr::bind_cols(base, other_man)
}


# "https://www.sec.gov/Archives/edgar/data/861177/000086117720000005/primary_doc.xml" %>%
#   parse_13f_meta_xml()



#' Retrieve Meta Information for a 13F filing.
#'
#' \code{get_13f_meta} retrieves and parses a company's 13F filings or the links to the filings.
#' @param cik Central index key of a company.
#' @param year Which year or time period of filings.
#' @param amendments Include 13F-HR/A when retrieving — only works when \code{link_only = TRUE}. Defaults to FALSE.
#' @param link_only Do not retrieve full filing but only link to it.
#' @examples
#' # Get and parse BlackRock's 2015 13F filings.
#' cik_blackrock <- "1364742"
#' get_13f_meta(cik = cik_blackrock, year = 2015, link_only = FALSE)
#' # Get the links to the Swiss National Bank's 2016 to 2019 filings.
#' cik_snb <- "1582202"
#' get_13f_meta(cik = cik_snb, year = 2016:2019, amendments = TRUE, link_only = TRUE)
#' @export

get_13f_meta <- function(cik, year, amendments = FALSE, link_only = FALSE) {
  assertive::assert_is_not_null(cik, severity = "stop")
  assertive::assert_is_not_null(year, severity = "stop")
  assertive::assert_is_not_null(amendments, severity = "stop")

  # conditionally ex- or include amendments to 13F-HR
  if (amendments) {
    filing_type_var <- c("13F-HR/A", "13F-HR")
  } else if (isFALSE(amendments)) {
    filing_type_var <- "13F-HR"
  }

  # make list of filings and conditionally subset
  filings_df <- get_list_of_filings(cik = cik, year = year, filing_type = filing_type_var)

  # conditional for when only the link is requested
  if (link_only) {
    filings_df %>%
      mutate(
        link_to_filing = paste0("https://www.sec.gov/Archives/", filename)
      ) %>%
      select(-filename) %>%
      dplyr::as_tibble()
  } else if (isFALSE(link_only)) {

    # make links to 13F XML
    links_to_13f <- get_13f_meta_link(filings_df)
    message(crayon::bgYellow("Now parsing meta"))

    # filings <- links_to_13f %>%
    #   dplyr::filter(endsWith(x = link_to_filing, suffix = "xml")) %>%
    #   dplyr::mutate(filing = purrr::map(link_to_filing, parse_13f_meta_xml)) %>%
    #   dplyr::as_tibble()
    #
    # message(emo::ji_glue(":+1:"), "Those filings are ready now, hehe.")
    # return(filings)
  }
  # # parse contents in link
  # filings <- links_to_13f %>%
  # mutate(filing = purrr::map(link_to_filing, parse_13f))

  # filings
}


# cik_blackrock <- "1364742"
# get_13f_meta(cik = cik_blackrock, year = 2015, amendments = FALSE,


#' Retrieve 13F filings
#'
#' \code{get_13f} retrieves and parses a company's 13F filings along with meta information or the links to the filings.
#' @param cik Central Index Key for filing entity, see https://www.sec.gov/edgar/searchedgar/companysearch.html
#' @param year Year for which filings should be retrieved.
#' @param amendments Include 13F-HR/A when retrieving — only works when \code{link_only = TRUE}. Defaults to FALSE.
#' @param link_only Do not retrieve full filing but only link to it.
#' @examples
#' # Get and parse BlackRock's 2015 13F filings.
#' cik_blackrock <- "1364742"
#' get_13f(
#'   cik = cik_blackrock, year = 2015,
#'   link_only = FALSE
#' )
#' # Get the links to the Swiss National Bank's 2016 to 2019 filings.
#' cik_snb <- "1582202"
#' snb <- get_13f(
#'   cik = cik_snb, year = 2016:2019,
#'   amendments = TRUE, link_only = TRUE
#' )
#' @export

get_13f <- function(cik, year, amendments = FALSE, link_only = FALSE) {
  assertive::assert_is_not_null(cik, severity = "stop")
  assertive::assert_is_not_null(year, severity = "stop")
  assertive::assert_is_not_null(amendments, severity = "stop")

  # conditionally ex- or include amendments to 13F-HR
  if (amendments) {
    filing_type_var <- c("13F-HR/A", "13F-HR")
  } else if (isFALSE(amendments)) {
    filing_type_var <- "13F-HR"
  }

  # make list of filings and conditionally subset
  filings_df <- get_list_of_filings(cik = cik, year = year, filing_type = filing_type_var)

  # conditional for when only the link is requested
  if (link_only) {
    filings_df %>%
      mutate(
        link_to_filing = paste0("https://www.sec.gov/Archives/", filename)
      ) %>%
      select(-filename) %>%
      dplyr::as_tibble()
  } else if (isFALSE(link_only)) {

    # make links to 13F XML
    links_to_13f <- get_13f_link(filings_df)
    message(crayon::bgYellow("Now parsing filings..."))

    filings <- links_to_13f %>%
      dplyr::filter(endsWith(x = link_to_filing, suffix = "xml")) %>%
      dplyr::mutate(filing = purrr::map(link_to_filing, parse_13f_submission_xml)) %>%
      dplyr::mutate(filing_number = dirname(link_to_filing) %>% basename()) %>%
      dplyr::as_tibble()

    # get primary_doc
    list_of_filings <- get_list_of_filings(cik = cik, year = year, filing_type = filing_type_var)
    primary_docs <- construct_link_to_filing_directory(dt = list_of_filings, xml_index = TRUE) %>%
      purrr::map_chr(check_for_and_get_xml_primary_doc_file) %>%
      purrr::map_df(parse_13f_meta_xml) %>%
      select(-cik)

    # # add content of primary_docs
    filings_plus_meta <- left_join(filings, primary_docs, by = c("filing_number"))
    message(emo::ji_glue(":+1:"), "Those filings are ready now, hehe.")

    return(filings_plus_meta)
  }
}
